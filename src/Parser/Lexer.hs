{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Lexer
-- Description : Hand-written scanner turning source text into a token stream.
--
-- The lexer is total and never gives up: a character it cannot make a token
-- out of is reported and skipped, so 'lexTokens' always returns a complete
-- stream (terminated by exactly one 'TEof') alongside whatever went wrong.
-- That is what lets the parser report /several/ problems in one pass instead
-- of dying on the first bad byte.
--
-- Positions are 1-indexed lines and columns counted in characters; a tab
-- counts as one column.
module Parser.Lexer
  ( lexTokens,
  )
where

import Ast (Pos (..), Span (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Error (ParseError (..), ParseErrorKind (..))
import Parser.Token

-- | Everything the scanner threads through the source, kept strict so a long
-- file does not build a chain of thunks for its position.
data LexState = LexState
  { lsRest :: !Text,
    lsPos :: !Pos,
    -- | accumulated in reverse
    lsToks :: ![Token],
    -- | accumulated in reverse
    lsErrs :: ![ParseError]
  }

-- | Scan an entire source file. The token list always ends with 'TEof'.
lexTokens :: FilePath -> Text -> ([Token], [ParseError])
lexTokens file src = finish (go (LexState src (Pos 1 1) [] []))
  where
    finish st =
      let eof = Token TEof (Span file (lsPos st) (lsPos st))
       in (reverse (eof : lsToks st), reverse (lsErrs st))

    go st =
      let st' = skipTrivia file st
       in if T.null (lsRest st')
            then st'
            else go (scanToken file st')

--------------------------------------------------------------------------------
-- Position arithmetic
--------------------------------------------------------------------------------

-- | Move a position past a single character.
step :: Pos -> Char -> Pos
step (Pos l _) '\n' = Pos (l + 1) 1
step (Pos l c) _ = Pos l (c + 1)

-- | Move a position past a run of characters.
stepOver :: Pos -> Text -> Pos
stepOver = T.foldl' step

-- | Consume @taken@ from the front of the input, advancing the position.
eat :: Text -> LexState -> LexState
eat taken st =
  st
    { lsRest = T.drop (T.length taken) (lsRest st),
      lsPos = stepOver (lsPos st) taken
    }

-- | Emit a token spanning @taken@, then consume it.
emit :: FilePath -> TokenKind -> Text -> LexState -> LexState
emit file kind taken st =
  let st' = eat taken st
      tok = Token kind (Span file (lsPos st) (lsPos st'))
   in st' {lsToks = tok : lsToks st'}

-- | Record an error spanning @taken@ without consuming anything.
report :: FilePath -> ParseErrorKind -> Text -> LexState -> LexState
report file kind taken st =
  let sp = Span file (lsPos st) (stepOver (lsPos st) taken)
   in st {lsErrs = ParseError sp kind [] : lsErrs st}

--------------------------------------------------------------------------------
-- Whitespace and comments
--------------------------------------------------------------------------------

-- | Skip whitespace, @\/\/@ line comments and @\/* *\/@ block comments until
-- the input starts with something that can begin a token.
skipTrivia :: FilePath -> LexState -> LexState
skipTrivia file st
  | not (T.null spaces) = skipTrivia file (eat spaces st)
  | "//" `T.isPrefixOf` rest = skipTrivia file (eat (T.takeWhile (/= '\n') rest) st)
  | "/*" `T.isPrefixOf` rest = skipBlockComment file st
  | otherwise = st
  where
    rest = lsRest st
    spaces = T.takeWhile isSpace rest

-- | Skip a block comment, reporting it if it is never closed. Block comments
-- do not nest, matching the previous grammar.
skipBlockComment :: FilePath -> LexState -> LexState
skipBlockComment file st
  | T.null after = skipTrivia file (eat (lsRest st) (report file UnterminatedComment "/*" st))
  | otherwise = skipTrivia file (eat (T.take (T.length body + 4) (lsRest st)) st)
  where
    (body, after) = T.breakOn "*/" (T.drop 2 (lsRest st))

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

-- | Scan exactly one token. Only called with a non-empty, trivia-free input.
scanToken :: FilePath -> LexState -> LexState
scanToken file st = case T.uncons (lsRest st) of
  Nothing -> st
  Just (c, _)
    | isIdentStart c -> scanIdent file st
    | isDigit c -> scanNumber file st
    | c == '"' || c == '\'' -> scanString file c st
    | otherwise -> case matchPunct (lsRest st) of
        Just (text, p) -> emit file (TPunct p) text st
        Nothing -> eat (T.singleton c) (report file (UnexpectedCharacter c) (T.singleton c) st)

-- | Identifiers may start with a letter, @_@ or @$@ and continue with those
-- plus digits, exactly as the previous grammar allowed.
isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_' || c == '$'

isIdentPart :: Char -> Bool
isIdentPart c = isAlphaNum c || c == '_' || c == '$'

-- | An identifier, promoted to a keyword when it is a reserved word.
scanIdent :: FilePath -> LexState -> LexState
scanIdent file st = emit file kind word st
  where
    word = T.takeWhile isIdentPart (lsRest st)
    kind = maybe (TIdent word) TKw (M.lookup word keywordTable)

-- | A decimal integer or float literal. Folded rather than @read@ so the
-- scanner stays total.
--
-- A @.@ only starts a fractional part when it is followed by at least one
-- digit: @5.@ lexes as the integer @5@ followed by a @.@ token, leaving
-- @5.toString@-shaped input to the member-access grammar rather than being
-- swallowed here.
scanNumber :: FilePath -> LexState -> LexState
scanNumber file st
  | isFloat = emit file (TFloatLit (digitsToDouble intDigits fracDigits)) (intDigits <> "." <> fracDigits) st
  | otherwise = emit file (TIntLit (digitsToInteger intDigits)) intDigits st
  where
    intDigits = T.takeWhile isDigit (lsRest st)
    afterInt = T.drop (T.length intDigits) (lsRest st)
    fracDigits = T.takeWhile isDigit (T.drop 1 afterInt)
    isFloat = "." `T.isPrefixOf` afterInt && not (T.null fracDigits)

digitsToInteger :: Text -> Integer
digitsToInteger = T.foldl' (\acc c -> acc * 10 + toInteger (fromEnum c - fromEnum '0')) 0

-- | Combine a whole-number and fractional digit run into a 'Double'. The
-- fractional part is folded from the right so each digit is weighted by the
-- correct negative power of ten purely through repeated division, with no
-- intermediate 'Integer' magnitude to convert.
digitsToDouble :: Text -> Text -> Double
digitsToDouble intDigits fracDigits =
  fromInteger (digitsToInteger intDigits) + T.foldr (\c acc -> (acc + digitVal c) / 10) 0 fracDigits
  where
    digitVal c = fromIntegral (fromEnum c - fromEnum '0')

-- | A string literal delimited by @\"@ or @'@. There are no escape sequences
-- in the language yet, so the literal simply runs to the next matching quote.
--
-- A literal with no closing quote is reported and then recovered rather than
-- abandoned: it ends at the end of its line, and a 'TStrLit' is emitted for
-- what was read. Both halves of that matter. Running to end of file instead
-- would put a caret under the whole remainder of the source and leave nothing
-- after it to lex, and dropping the token would make the parser complain a
-- second time about the expression that is now missing.
scanString :: FilePath -> Char -> LexState -> LexState
scanString file quote st
  | T.null after = emit file (TStrLit unclosed) opened (report file UnterminatedString opened st)
  | otherwise = emit file (TStrLit body) (T.concat [q, body, q]) st
  where
    q = T.singleton quote
    contents = T.drop 1 (lsRest st)
    (body, after) = T.break (== quote) contents
    unclosed = T.takeWhile (/= '\n') contents
    opened = T.cons quote unclosed

-- | Maximal-munch punctuation match against the length-ordered table.
matchPunct :: Text -> Maybe (Text, Punct)
matchPunct input = find (\(text, _) -> text `T.isPrefixOf` input) punctTable
