{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Monad
-- Description : The parser's state, its error-recovery machinery, and the token primitives.
--
-- == The monad
--
-- @'Parser' = 'ExceptT' 'Recovering' ('State' 'ParserState')@, in @mtl@ style
-- so the grammar in "Parser" can be written against 'MonadState' /
-- 'MonadError' without naming the concrete stack.
--
-- The 'ExceptT' layer is deliberately /not/ how errors are reported. An error
-- is reported by appending it to 'psErrors'; throwing 'Recovering' afterwards
-- only abandons the half-built construct and unwinds to the nearest recovery
-- point. That separation is what makes multiple errors per pass possible: the
-- error survives in the state even though the computation that produced it
-- was discarded.
--
-- == Panic mode
--
-- A recovery point is any call to 'recover' — in practice, the statement loop
-- and each bracketed group. On catching 'Recovering' the statement loop calls
-- 'synchronize', which discards tokens until the stream is plausibly at the
-- start of another statement (after a @;@, at a statement keyword, or past the
-- @{ … }@ body of the construct that failed). A bracketed group instead calls
-- 'skipToClose' and carries on immediately after its own closing delimiter, so
-- that a malformed parameter list does not cost the diagnostics in the body
-- that follows it.
--
-- Both are structure-aware on purpose. Resuming in the middle of a block whose
-- header failed would parse that block's statements at the wrong nesting level
-- and then report its closing @}@ as an unexpected token — a second error for
-- what is really one mistake.
--
-- 'psConsumed' exists purely to let the statement loop prove it is making
-- progress: if a failed statement plus a synchronise consumed nothing at all,
-- the loop forces a single 'advance' rather than spinning.
module Parser.Monad
  ( -- * The monad
    Parser,
    ParserState (..),
    Context (..),
    Recovering (..),
    runParser,

    -- * Inspecting the stream
    peek,
    peekKind,
    check,
    atEof,
    lambdaAhead,

    -- * Consuming the stream
    advance,
    match,
    expect,
    expectWith,
    expectIdent,
    expectUpperIdent,
    expectString,

    -- * Spans
    withSpan,
    spanning,
    isUpperName,

    -- * Errors and recovery
    Nesting (..),
    parseError,
    parseErrorWith,
    recordError,
    recover,
    panic,
    skipToClose,
    synchronize,
  )
where

import Ast (Located (..), Name, Pos (..), Span (..))
import Control.Monad (unless, void, when)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char (isUpper, toUpper)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Diagnostic.Types (Note (..))
import Parser.Error (ParseError (..), ParseErrorKind (..))
import Parser.Token

-- | The construct currently being parsed, spliced into every error as a
-- @note: while parsing …@ line. Threading it explicitly (rather than through a
-- reader layer) keeps each error's phrasing visible at the site that raises it.
newtype Context = Context Text
  deriving newtype (Eq, Show)

-- | Raised to abandon a construct whose error has already been recorded.
-- It carries no payload on purpose: the diagnostic lives in 'psErrors', and
-- anything that catches this only needs to know that it must resynchronise.
data Recovering = Recovering
  deriving stock (Eq, Show)

-- | Everything the parser threads through a parse.
--
-- The lookahead token is held separately from the rest of the stream so that
-- 'peek' is total: there is always a current token, and once it is 'TEof' the
-- parser can no longer move.
data ParserState = ParserState
  { -- | file the tokens came from, for building spans
    psFile :: !FilePath,
    -- | the single-token lookahead
    psCurrent :: !Token,
    -- | the remaining stream after 'psCurrent'
    psRest :: ![Token],
    -- | end of the most recently consumed token, i.e. where the node being
    -- parsed currently ends
    psPrevEnd :: !Pos,
    -- | tokens consumed so far; only ever compared, never displayed
    psConsumed :: !Int,
    -- | errors accumulated so far, most recent first
    psErrors :: ![ParseError]
  }

newtype Parser a = Parser (ExceptT Recovering (State ParserState) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError Recovering,
      MonadState ParserState
    )

-- | Run a parser over a token stream, seeding the error list with whatever the
-- lexer already found. Returns 'Nothing' only if the parser panicked without
-- ever reaching a recovery point.
runParser :: FilePath -> [Token] -> [ParseError] -> Parser a -> (Maybe a, [ParseError])
runParser file toks lexErrs (Parser p) =
  let (result, st) = runState (runExceptT p) initial
   in (either (const Nothing) Just result, reverse (psErrors st))
  where
    (current, rest) = case toks of
      (t : ts) -> (t, ts)
      -- 'lexTokens' always terminates the stream with 'TEof', so this only
      -- guards against a caller synthesising an empty stream by hand.
      [] -> (Token TEof (Span file (Pos 1 1) (Pos 1 1)), [])
    initial =
      ParserState
        { psFile = file,
          psCurrent = current,
          psRest = rest,
          psPrevEnd = spanStart (tokSpan current),
          psConsumed = 0,
          psErrors = reverse lexErrs
        }

--------------------------------------------------------------------------------
-- Inspecting the stream
--------------------------------------------------------------------------------

-- | The lookahead token, without consuming it.
peek :: Parser Token
peek = gets psCurrent

peekKind :: Parser TokenKind
peekKind = tokKind <$> peek

-- | Is the lookahead token exactly this kind?
check :: TokenKind -> Parser Bool
check k = (== k) <$> peekKind

atEof :: Parser Bool
atEof = check TEof

-- | Does the parenthesised group starting at the lookahead token introduce a
-- lambda rather than a parenthesised expression?
--
-- This is the one place the grammar is not LL(1): @(a, b)@ is the start of
-- both @(a, b) => a + b@ and @(a, b)@ — well, of a parenthesised expression in
-- general. Rather than backtrack, scan forward for the matching @)@ and look
-- at what follows it: only a lambda can continue with @=>@ or with a @:@
-- return-type annotation, since neither is legal after a parenthesised
-- expression.
lambdaAhead :: Parser Bool
lambdaAhead = do
  st <- get
  pure (scan (0 :: Int) (psCurrent st : psRest st))
  where
    scan depth = \case
      [] -> False
      (t : ts) -> case tokKind t of
        TEof -> False
        TPunct LParen -> scan (depth + 1) ts
        TPunct RParen
          | depth <= 1 -> introducesLambda ts
          | otherwise -> scan (depth - 1) ts
        _ -> scan depth ts

    introducesLambda (t : _) = tokKind t `elem` [TPunct FatArrow, TPunct Colon]
    introducesLambda [] = False

--------------------------------------------------------------------------------
-- Consuming the stream
--------------------------------------------------------------------------------

-- | Consume the lookahead token and return it, remembering where it ended.
--
-- At 'TEof' this is a no-op that still reports the end token, so any loop that
-- advances until some condition holds is guaranteed to terminate.
advance :: Parser Token
advance = do
  st <- get
  let current = psCurrent st
      ending = st {psPrevEnd = spanEnd (tokSpan current)}
  put $ case psRest st of
    (next : rest) ->
      ending {psCurrent = next, psRest = rest, psConsumed = psConsumed st + 1}
    [] -> ending
  pure current

-- | Consume the lookahead token if it is this kind, reporting whether it was.
match :: TokenKind -> Parser Bool
match k = do
  hit <- check k
  when hit (void advance)
  pure hit

-- | Consume the lookahead token, which must be this kind.
--
-- On a mismatch this records an @expected …, found …@ error tagged with
-- 'Context' and enters panic mode; it never returns normally.
expect :: Context -> TokenKind -> Parser Token
expect ctx = expectWith ctx []

-- | 'expect' with extra notes attached to the error it might raise.
expectWith :: Context -> [Note] -> TokenKind -> Parser Token
expectWith ctx notes k = do
  hit <- check k
  if hit
    then advance
    else do
      found <- describeFound
      parseErrorWith ctx (UnexpectedToken [describeExpected k] found) notes

-- | Consume any identifier.
expectIdent :: Context -> Parser (Located Name)
expectIdent ctx = do
  t <- peek
  case tokKind t of
    TIdent n -> advance $> Located (tokSpan t) n
    TKw k ->
      parseErrorWith
        ctx
        (UnexpectedToken ["an identifier"] (describeKind (tokKind t)))
        [NoteHelp ("`" <> keywordText k <> "` is a reserved word and cannot be used as a name")]
    other -> parseError ctx (UnexpectedToken ["an identifier"] (describeKind other))

-- | Consume an identifier that names a type, enum or variant, all of which
-- must be capitalised.
expectUpperIdent :: Context -> Parser (Located Name)
expectUpperIdent ctx = do
  t <- peek
  case tokKind t of
    TIdent n
      | isUpperName n -> advance $> Located (tokSpan t) n
      | otherwise ->
          parseErrorWith
            ctx
            (UnexpectedToken ["a capitalised name"] (describeKind (tokKind t)))
            [NoteHelp ("type, enum and variant names must start with an uppercase letter, e.g. `" <> capitalise n <> "`")]
    other -> parseError ctx (UnexpectedToken ["a capitalised name"] (describeKind other))

-- | Consume a string literal.
expectString :: Context -> Parser (Located Text)
expectString ctx = do
  t <- peek
  case tokKind t of
    TStrLit s -> advance $> Located (tokSpan t) s
    other -> parseError ctx (UnexpectedToken ["a string literal"] (describeKind other))

isUpperName :: Name -> Bool
isUpperName n = case T.uncons n of
  Just (c, _) -> isUpper c
  Nothing -> False

capitalise :: Name -> Name
capitalise n = case T.uncons n of
  Just (c, cs) -> T.cons (toUpper c) cs
  Nothing -> n

--------------------------------------------------------------------------------
-- Spans
--------------------------------------------------------------------------------

-- | Run @p@ and annotate its result with the span it consumed: from the start
-- of the token that was the lookahead when @p@ began, to the end of the last
-- token @p@ consumed.
--
-- Unlike the previous lexeme-based parser, the end position stops at the final
-- character of the node rather than after the whitespace behind it, so a caret
-- underline covers exactly the offending source text.
withSpan :: Parser a -> Parser (Located a)
withSpan p = do
  st <- get
  let start = spanStart (tokSpan (psCurrent st))
  x <- p
  end <- gets psPrevEnd
  pure (Located (Span (psFile st) start end) x)

-- | The span reaching from the start of the first to the end of the second.
spanning :: Span -> Span -> Span
spanning a b = Span (spanFile a) (spanStart a) (spanEnd b)

--------------------------------------------------------------------------------
-- Errors and recovery
--------------------------------------------------------------------------------

-- | Record an error at the lookahead token and abandon the current construct.
parseError :: Context -> ParseErrorKind -> Parser a
parseError ctx kind = parseErrorWith ctx kind []

-- | 'parseError' with additional notes.
parseErrorWith :: Context -> ParseErrorKind -> [Note] -> Parser a
parseErrorWith ctx kind notes = do
  recordError ctx kind notes
  throwError Recovering

-- | Record an error without abandoning the parse, for the cases where the
-- parser can sensibly keep going.
--
-- Two kinds of follow-on noise are dropped here. After a failure the parser is
-- by definition confused, so a second complaint about the same token says
-- nothing new; and an @expected …, found end of file@ in a file that already
-- has an error is a report that the source ran out while the parser was
-- lost, not a report of an independent mistake. A genuinely unclosed
-- delimiter is a different diagnostic ('UnclosedDelimiter') and still stands
-- on its own at end of file.
recordError :: Context -> ParseErrorKind -> [Note] -> Parser ()
recordError (Context what) kind notes = do
  st <- get
  let sp = tokSpan (psCurrent st)
      err = ParseError sp kind (NoteText ("while parsing " <> what) : notes)
      redundant = alreadyReportedAt sp (psErrors st) || ranOut st
  unless redundant $ put st {psErrors = err : psErrors st}
  where
    alreadyReportedAt sp (e : _) = spanStart (peSpan e) == spanStart sp
    alreadyReportedAt _ [] = False

    ranOut st = case (kind, tokKind (psCurrent st)) of
      (UnexpectedToken _ _, TEof) -> not (null (psErrors st))
      _ -> False

-- | Describe the lookahead token for an @…, found X@ message.
describeFound :: Parser Text
describeFound = describeKind <$> peekKind

-- | Establish a recovery point: run @p@, turning a panic into 'Nothing'.
recover :: Parser a -> Parser (Maybe a)
recover p = (Just <$> p) `catchError` \Recovering -> pure Nothing

-- | Abandon the current construct without recording anything new, for the
-- cases where the diagnostic is already in 'psErrors' and only the unwinding
-- is still wanted.
panic :: Parser a
panic = throwError Recovering

-- | Whether the statement loop being recovered is a file's top level or the
-- inside of a @{ … }@ block. It decides what a @}@ means during panic mode:
-- inside a block it terminates the loop, at the top level it closes nothing
-- and is discarded.
data Nesting = TopLevel | InsideBlock
  deriving stock (Eq, Show)

-- | Discard tokens up to and including the delimiter that closes the group the
-- parser is currently inside, reporting whether it was found.
--
-- Nested bracketed groups are skipped whole, and a closing delimiter belonging
-- to an /enclosing/ group stops the scan without being consumed — recovery
-- must never eat a bracket that an outer construct is still waiting for. A
-- 'False' result therefore means the caller cannot resume locally and should
-- 'panic' on to the next recovery point.
skipToClose :: Punct -> Parser Bool
skipToClose close = go (0 :: Int)
  where
    go depth =
      peekKind >>= \case
        TEof -> pure False
        TPunct p
          | p == close && depth == 0 -> advance $> True
          | isOpener p -> advance >> go (depth + 1)
          | isCloser p && depth == 0 -> pure False
          | isCloser p -> advance >> go (depth - 1)
        _ -> advance >> go depth

-- | Discard a whole balanced @{ … }@ group, the lookahead being its @{@.
skipBlock :: Parser ()
skipBlock = advance >> go (0 :: Int)
  where
    go depth =
      peekKind >>= \case
        TEof -> pure ()
        TPunct LBrace -> advance >> go (depth + 1)
        TPunct RBrace
          | depth == 0 -> void advance
          | otherwise -> advance >> go (depth - 1)
        _ -> advance >> go depth

-- | Panic mode. Discard tokens until the stream is plausibly at the start of
-- the next statement.
--
-- A @;@ is consumed, since it terminated the statement we gave up on, and any
-- statement keyword is left in place to be parsed as the next statement.
--
-- The other two cases exist to stop one syntax error from being reported
-- twice. A @{@ opens the body of the construct that just failed — a function
-- whose parameter list was malformed, say — so the whole balanced group is
-- discarded with it; resuming inside that body would parse its statements at
-- the wrong nesting level and then blame its closing @}@ for being unexpected.
-- A @}@ at the top level is the other half of the same situation: nothing is
-- open for it to close, so it is dropped rather than reported.
synchronize :: Nesting -> Parser ()
synchronize nesting = go
  where
    go =
      peekKind >>= \case
        TEof -> pure ()
        TPunct Semi -> void advance
        TPunct LBrace -> skipBlock
        TPunct RBrace -> case nesting of
          InsideBlock -> pure ()
          TopLevel -> advance >> go
        TKw k | startsStatement k -> pure ()
        _ -> advance >> go
