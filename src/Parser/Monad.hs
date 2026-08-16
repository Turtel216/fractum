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
-- A recovery point is any call to 'recover' — in practice, the statement loop.
-- On catching 'Recovering' it calls 'synchronize', which discards tokens until
-- the stream is plausibly at the start of another statement (after a @;@, at a
-- @}@, or at a statement keyword). Parsing then resumes normally.
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
    parseError,
    parseErrorWith,
    recordError,
    recover,
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
-- Errors at a position that already has one are dropped. After a failure the
-- parser is by definition confused, and a cascade of complaints about the same
-- token is noise rather than information.
recordError :: Context -> ParseErrorKind -> [Note] -> Parser ()
recordError (Context what) kind notes = do
  st <- get
  let sp = tokSpan (psCurrent st)
      err = ParseError sp kind (NoteText ("while parsing " <> what) : notes)
  unless (alreadyReportedAt sp (psErrors st)) $
    put st {psErrors = err : psErrors st}
  where
    alreadyReportedAt sp (e : _) = spanStart (peSpan e) == spanStart sp
    alreadyReportedAt _ [] = False

-- | Describe the lookahead token for an @…, found X@ message.
describeFound :: Parser Text
describeFound = describeKind <$> peekKind

-- | Establish a recovery point: run @p@, turning a panic into 'Nothing'.
recover :: Parser a -> Parser (Maybe a)
recover p = (Just <$> p) `catchError` \Recovering -> pure Nothing

-- | Panic mode. Discard tokens until the stream is plausibly at the start of
-- the next statement.
--
-- A @;@ is consumed (it terminated the statement we gave up on), while a @}@
-- and any statement keyword are left in place for the enclosing block or
-- statement loop to handle.
synchronize :: Parser ()
synchronize = do
  t <- peek
  case tokKind t of
    TEof -> pure ()
    TPunct Semi -> void advance
    TPunct RBrace -> pure ()
    TKw k | startsStatement k -> pure ()
    _ -> advance >> synchronize
