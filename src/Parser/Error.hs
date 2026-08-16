{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Error
-- Description : Syntax errors, shaped like the typechecker's.
--
-- A 'ParseError' carries the same payload a 'Typecheck.Error.TypeError' does —
-- a primary span, a classified kind, and a list of contextual notes — so that
-- both render through the identical code path in "Diagnostic" and read
-- identically to the user. Syntax errors are numbered @P0001@ upwards to keep
-- them distinguishable from the typechecker's @E@ codes at a glance.
module Parser.Error
  ( ParseError (..),
    ParseErrorKind (..),
    orList,
  )
where

import Ast (Span)
import Data.Text (Text)
import qualified Data.Text as T
import Diagnostic.Types (Diagnostic (..), Note (..), ToDiagnostic (..))

-- | A syntax error with its source location and contextual notes.
data ParseError = ParseError
  { -- | the offending token's span
    peSpan :: !Span,
    -- | what went wrong
    peKind :: !ParseErrorKind,
    -- | secondary explanations, e.g. which construct was being parsed
    peNotes :: ![Note]
  }
  deriving (Eq, Show)

-- | Classification of syntax errors.
data ParseErrorKind
  = -- | what the parser would have accepted here, and what it actually found
    UnexpectedToken [Text] Text
  | -- | opening delimiter, and the closing delimiter that never arrived
    UnclosedDelimiter Text Text
  | -- | a parenthesised type list that is not followed by @->@
    DanglingParamList
  | -- | string literal running past the end of the file
    UnterminatedString
  | -- | block comment running past the end of the file
    UnterminatedComment
  | -- | a character that begins no token at all
    UnexpectedCharacter Char
  deriving (Eq, Show)

instance ToDiagnostic ParseError where
  toDiagnostic (ParseError sp kind notes) =
    Diagnostic
      { diagCode = errorCode kind,
        diagTitle = errorTitle kind,
        diagSpan = Just sp,
        diagDetail = errorDetail kind,
        diagNotes = notes
      }

-- | Stable error code for each error kind.
errorCode :: ParseErrorKind -> Text
errorCode = \case
  UnexpectedToken {} -> "P0001"
  UnclosedDelimiter {} -> "P0002"
  DanglingParamList {} -> "P0003"
  UnterminatedString {} -> "P0004"
  UnterminatedComment {} -> "P0005"
  UnexpectedCharacter {} -> "P0006"

-- | Short human-readable title for the error kind.
errorTitle :: ParseErrorKind -> Text
errorTitle = \case
  UnexpectedToken {} -> "unexpected token"
  UnclosedDelimiter {} -> "unclosed delimiter"
  DanglingParamList {} -> "expected a function type"
  UnterminatedString {} -> "unterminated string literal"
  UnterminatedComment {} -> "unterminated block comment"
  UnexpectedCharacter {} -> "unexpected character"

-- | Detailed message shown under the source underline.
errorDetail :: ParseErrorKind -> Text
errorDetail = \case
  UnexpectedToken expected found ->
    "expected " <> orList expected <> ", found " <> found
  UnclosedDelimiter open close ->
    "expected `" <> close <> "` to close this `" <> open <> "`"
  DanglingParamList ->
    "expected `->` after a parenthesised parameter list"
  UnterminatedString ->
    "this string literal is missing its closing quote"
  UnterminatedComment ->
    "this block comment is missing its closing `*/`"
  UnexpectedCharacter c ->
    "`" <> T.singleton c <> "` does not start any token"

-- | Join alternatives the way prose does: @a@, @a or b@, @a, b, or c@.
orList :: [Text] -> Text
orList xs = case reverse xs of
  [] -> "something else"
  [x] -> x
  [y, x] -> x <> " or " <> y
  (y : rest) -> T.intercalate ", " (reverse rest) <> ", or " <> y
