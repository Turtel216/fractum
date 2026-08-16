{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Token
-- Description : The lexical vocabulary of Fractum.
--
-- Splitting lexing from parsing (the previous @megaparsec@ grammar was
-- scannerless) buys the recursive-descent parser two things it needs for good
-- errors: single-token lookahead that never has to backtrack over raw
-- characters, and a token span that stops exactly at the last character of the
-- token rather than after the trailing whitespace a lexeme combinator eats.
--
-- Both the keyword table and the punctuation table are derived from
-- 'Bounded'/'Enum' rather than written out twice, so adding a token means
-- adding one constructor and one line of spelling.
module Parser.Token
  ( Token (..),
    TokenKind (..),
    Keyword (..),
    Punct (..),
    keywordText,
    keywordTable,
    punctText,
    punctTable,
    startsStatement,
    describeKind,
    describeExpected,
  )
where

import Ast (Name, Span)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Diagnostic.Types (tshow)

-- | A single lexed token together with the source it came from.
data Token = Token
  { tokKind :: !TokenKind,
    tokSpan :: !Span
  }
  deriving (Eq, Show)

-- | What a token is.
--
-- Keywords and punctuation are grouped into their own enumerations so that
-- 'TokenKind' stays small and so that @expect@ can compare a whole token kind
-- for equality; identifiers and literals carry their payload directly.
data TokenKind
  = TIdent !Name
  | TIntLit !Integer
  | TStrLit !Text
  | TKw !Keyword
  | TPunct !Punct
  | -- | end of input; the stream always ends with exactly one of these
    TEof
  deriving (Eq, Show)

-- | Reserved words. These can never be used as identifiers.
data Keyword
  = KwLet
  | KwMut
  | KwFunction
  | KwReturn
  | KwIf
  | KwElse
  | KwWhile
  | KwTrue
  | KwFalse
  | KwNull
  | KwType
  | KwEnum
  | KwMatch
  | KwImport
  | KwExport
  | KwFrom
  | KwAs
  | KwInt
  | KwBool
  | KwString
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The exact source spelling of a keyword.
keywordText :: Keyword -> Text
keywordText = \case
  KwLet -> "let"
  KwMut -> "mut"
  KwFunction -> "function"
  KwReturn -> "return"
  KwIf -> "if"
  KwElse -> "else"
  KwWhile -> "while"
  KwTrue -> "true"
  KwFalse -> "false"
  KwNull -> "null"
  KwType -> "type"
  KwEnum -> "enum"
  KwMatch -> "match"
  KwImport -> "import"
  KwExport -> "export"
  KwFrom -> "from"
  KwAs -> "as"
  KwInt -> "Int"
  KwBool -> "Bool"
  KwString -> "String"

-- | Lookup table used by the lexer to promote an identifier to a keyword.
keywordTable :: M.Map Text Keyword
keywordTable = M.fromList [(keywordText k, k) | k <- [minBound .. maxBound]]

-- | Operators and delimiters.
data Punct
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Semi
  | Colon
  | ColonColon
  | Dot
  | -- | @->@
    Arrow
  | -- | @=>@
    FatArrow
  | -- | @=@
    Assign
  | EqEq
  | BangEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | AmpAmp
  | PipePipe
  | Bang
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The exact source spelling of a punctuation token.
punctText :: Punct -> Text
punctText = \case
  LParen -> "("
  RParen -> ")"
  LBrace -> "{"
  RBrace -> "}"
  LBracket -> "["
  RBracket -> "]"
  Comma -> ","
  Semi -> ";"
  Colon -> ":"
  ColonColon -> "::"
  Dot -> "."
  Arrow -> "->"
  FatArrow -> "=>"
  Assign -> "="
  EqEq -> "=="
  BangEq -> "!="
  Less -> "<"
  LessEq -> "<="
  Greater -> ">"
  GreaterEq -> ">="
  Plus -> "+"
  Minus -> "-"
  Star -> "*"
  Slash -> "/"
  Percent -> "%"
  AmpAmp -> "&&"
  PipePipe -> "||"
  Bang -> "!"

-- | Every punctuation spelling, longest first, so that a linear scan is a
-- maximal-munch match: @->@ beats @-@, @::@ beats @:@, @>=@ beats @>@.
--
-- Note that no token spells @>>@; nested generics such as @Option\<Cmd\<Msg\>\>@
-- therefore close with two separate 'Greater' tokens, exactly as the
-- scannerless grammar used to see them.
punctTable :: [(Text, Punct)]
punctTable = sortOn (Down . T.length . fst) [(punctText p, p) | p <- [minBound .. maxBound]]

-- | Keywords that may begin a statement. Used both to dispatch in the
-- statement parser and as the synchronisation set for panic-mode recovery.
startsStatement :: Keyword -> Bool
startsStatement = \case
  KwLet -> True
  KwFunction -> True
  KwReturn -> True
  KwIf -> True
  KwWhile -> True
  KwType -> True
  KwEnum -> True
  KwImport -> True
  KwExport -> True
  _ -> False

-- | How a token is named when reporting what was actually found.
describeKind :: TokenKind -> Text
describeKind = \case
  TIdent n -> "identifier `" <> n <> "`"
  TIntLit n -> "integer literal `" <> tshow n <> "`"
  TStrLit _ -> "a string literal"
  TKw k -> "keyword `" <> keywordText k <> "`"
  TPunct p -> "`" <> punctText p <> "`"
  TEof -> "end of file"

-- | How a token is named when listing what the parser expected.
describeExpected :: TokenKind -> Text
describeExpected = \case
  TIdent _ -> "an identifier"
  TIntLit _ -> "an integer literal"
  TStrLit _ -> "a string literal"
  TKw k -> "`" <> keywordText k <> "`"
  TPunct p -> "`" <> punctText p <> "`"
  TEof -> "end of file"
