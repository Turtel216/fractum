{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser
-- Description : Megaparsec-based syntactic analysis and AST generation for TypedJs.
-- Stability   : experimental
--
-- This module implements a unified lexer and parser for the "TypedJs" surface language
-- using the @megaparsec@ library. It translates raw source text into a highly localized
-- surface AST ('Program', 'LExpr', 'LStmt'), tracking source spans to
-- feed the semantic analyzers error reporting.
--
-- == Architecture Overview
--
-- * __Lexical Analysis:__
--     Lexing and parsing are interleaved. Whitespace and comments are consumed
--     automatically *after* every token using the space consumer ('sc') and 'lexeme'
--     wrappers. This means parsers typically expect no leading whitespace but will
--     consume trailing whitespace.
--
-- * __Precise Span Tracking ('Located'):__
--     AST nodes are aggresively wrapped in 'Located' via the 'withSpan' combinator.
--     Because trailing whitespace is consumed by 'lexeme', the end positions recorded
--     by 'getSourcePos' may slightly overshoot the literal token boundary, but the
--     start bounds are strictly precise.
--
-- * __Operator Precedence & Associativity:__
--     Instead of using @makeExprParser@, operator precedence is encoded structurally
--     via layered parser combinators ('pAssign' -> 'pLogicOr' -> 'pAdditive', etc.).
--     This manual stratification allows a custom 'chainl1' to correctly capture and
--     merge the source spans of the LHS and RHS into a unified 'LExpr' bound.
--
-- * __Left-Recursion via Postfix Chains:__
--     Function calls, member access (@.foo@), and array indexing (@[i]@) are handled
--     by 'pPostfix'. It evaluates a base 'pPrimary' expression and then iteratively
--     consumes postfix operations, continually expanding the 'Span' from the original
--     base expression to the end of the most recently consumed postfix token.
--
-- * __Type Syntax:__
--     The parser natively supports type annotations, generic instantiations ('TApp'),
--     record shapes ('TObject'), array types, and structural function types.
--
-- == Entry Points
--
-- * 'parseProgram': The primary entry point. Consumes the entire input stream,
--     enforces 'eof', and maps Megaparsec's error bundle into a standard 'String'
--     on failure.
module Parser where

import Ast
import Control.Monad (void)
import Data.Char (isUpper)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Void as V
import Text.Megaparsec
  ( Parsec,
    between,
    choice,
    eof,
    getSourcePos,
    many,
    notFollowedBy,
    optional,
    runParser,
    satisfy,
    sepBy,
    sepBy1,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser combinator
type Parser = Parsec V.Void Text

-- | Convert a megaparsec 'SourcePos' to our 'Pos' type.
toPos :: MP.SourcePos -> Pos
toPos sp = Pos (MP.unPos (MP.sourceLine sp)) (MP.unPos (MP.sourceColumn sp))

-- | Run parser @p@ and wrap the result in 'Located' with the source span.
--
-- Note: the end position is taken *after* the parser has consumed trailing
-- whitespace (via 'lexeme'/'sc'), so spans may be slightly wider than the
-- actual token.  The start position is always accurate.
withSpan :: Parser a -> Parser (Located a)
withSpan p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  pure (Located (Span (toPos start) (toPos end)) x)

-- | Parse complete program
parseProgram :: FilePath -> Text -> Either String Program
parseProgram fp src =
  case runParser (sc *> pProgram <* eof) fp src of
    Left e -> Left (MP.errorBundlePretty e)
    Right x -> Right x

-- | Ignores spaces and comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | Helper for parsing a lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a given symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Consume parenthesis '('
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Consume braces '{'
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Consume brackets '['
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Consume semicolone ';'
semi :: Parser Text
semi = symbol ";"

-- | Consume comma ','
comma :: Parser Text
comma = symbol ","

-- | Consume colon ':'
colon :: Parser Text
colon = symbol ":"

-- | Consume arrow symbol '=>'
arrow :: Parser Text
arrow = symbol "=>"

-- | TypedJs reserved keywords
reserved :: [Text]
reserved =
  [ "let",
    "mut",
    "function",
    "return",
    "if",
    "else",
    "while",
    "true",
    "false",
    "null",
    "type",
    "enum",
    "match",
    "Int",
    "Bool",
    "String"
  ]

-- | Parse identifier
identifier :: Parser Text
identifier = lexeme . try $ do
  x <- letterChar <|> char '_' <|> char '$'
  xs <- many (alphaNumChar <|> char '_' <|> char '$')
  let n = T.pack (x : xs)
  if n `elem` reserved
    then fail ("reserved word " <> T.unpack n)
    else pure n

-- | Parse an uppercase identifier (for type names like Point, Pair)
upperIdentifier :: Parser Text
upperIdentifier = lexeme . try $ do
  x <- satisfy (\c -> isUpper c)
  xs <- many (alphaNumChar <|> char '_' <|> char '$')
  let n = T.pack (x : xs)
  if n `elem` reserved
    then fail ("reserved word " <> T.unpack n)
    else pure n

rword :: Text -> Parser ()
rword w = lexeme . try $ string w *> notFollowedBy identTail
  where
    identTail = alphaNumChar <|> char '_' <|> char '$'

-- | Parse String literal. Allows both "" and '', similar to Javascript
stringLit :: Parser Text
stringLit = lexeme $ do
  q <- char '"' <|> char '\''
  content <- many (satisfy (/= q))
  void (char q)
  pure (T.pack content)

-- | Parse integer
integer :: Parser Integer
integer = lexeme L.decimal

pProgram :: Parser Program
pProgram = Program <$> many pStmt

-- | Parse statement
pStmt :: Parser LStmt
pStmt =
  choice
    [ try (withSpan pEnumDecl),
      try (withSpan pTypeDecl),
      try (withSpan pFunDecl),
      try (withSpan pLet),
      withSpan pReturn,
      withSpan pIf,
      withSpan pWhile,
      withSpan (SBlock <$> pBlock),
      withSpan pExprStmt
    ]

-- | Parse Code block
pBlock :: Parser Block
pBlock = Block <$> braces (many pStmt)

-- | Parse let statement. Supports optional @mut@ qualifier for mutable bindings.
pLet :: Parser Stmt
pLet = do
  rword "let"
  mutFlag <- MP.option Immutable (rword "mut" $> Mutable)
  n <- identifier
  ty <- optional (colon *> pType)
  void (symbol "=")
  e <- pExpr
  void semi
  pure (SLet mutFlag n ty e)

-- | Parse type alias declaration: @type Point = { x: Int, y: Int };@
-- Also supports parametric aliases: @type Pair\<A, B\> = { first: A, second: B };@
pTypeDecl :: Parser Stmt
pTypeDecl = do
  rword "type"
  name <- upperIdentifier
  params <- MP.option [] (angles (identifier `sepBy1` comma))
  void (symbol "=")
  body <- pType
  void semi
  pure (STypeDecl name params body)
  where
    angles = between (symbol "<") (symbol ">")

-- | Parse enum declaration: @enum Shape { Circle(Int), Point }@
-- Also supports parametric enums: @enum Option\<T\> { Some(T), None }@
pEnumDecl :: Parser Stmt
pEnumDecl = do
  rword "enum"
  name <- upperIdentifier
  params <- MP.option [] (angles (identifier `sepBy1` comma))
  variants <- braces (pVariant `MP.sepEndBy` comma)
  pure (SEnum name params variants)
  where
    angles = between (symbol "<") (symbol ">")
    pVariant = do
      vname <- upperIdentifier
      fields <- MP.option [] (parens (pType `sepBy` comma))
      pure (Variant vname fields)

-- | Parse Declaration
pFunDecl :: Parser Stmt
pFunDecl = do
  rword "function"
  n <- identifier
  tyParams <- MP.option [] (angles (identifier `sepBy1` comma))
  params <- parens (pParam `sepBy` comma)
  retTy <- optional (colon *> pType)
  b <- pBlock
  pure (SFun n tyParams params retTy b)
  where
    angles = between (symbol "<") (symbol ">")

-- | Parse return statement
pReturn :: Parser Stmt
pReturn = do
  rword "return"
  e <- optional pExpr
  void semi
  pure (SReturn e)

-- | Parse if statement
pIf :: Parser Stmt
pIf = do
  rword "if"
  cond <- parens pExpr
  th <- pBlock
  el <- optional (rword "else" *> pBlock)
  pure (SIf cond th el)

-- | Parse while loop
pWhile :: Parser Stmt
pWhile = do
  rword "while"
  cond <- parens pExpr
  b <- pBlock
  pure (SWhile cond b)

-- | Parse Expression Statement
pExprStmt :: Parser Stmt
pExprStmt = do
  e <- pExpr
  void semi
  pure (SExpr e)

-- | Parse function parameters
pParam :: Parser Param
pParam = do
  n <- identifier
  ty <- optional (colon *> pType)
  pure (Param n ty)

-- | Parse Expression
pExpr :: Parser LExpr
pExpr = pAssign

-- | Parse Assignment
pAssign :: Parser LExpr
pAssign = do
  lhs <- pLogicOr
  optional (symbol "=") >>= \case
    Nothing -> pure lhs
    Just _ -> do
      rhs <- pAssign
      let sp = Span (spanStart (locSpan lhs)) (spanEnd (locSpan rhs))
      pure (Located sp (EAssign lhs rhs))

-- | Parse Logical Or
pLogicOr :: Parser LExpr
pLogicOr = chainl1 pLogicAnd (symbol "||" $> EBinary Or)

-- | Parse Logical And
pLogicAnd :: Parser LExpr
pLogicAnd = chainl1 pEquality (symbol "&&" $> EBinary And)

-- | Parse Equality expression. Both '==' and '!='
pEquality :: Parser LExpr
pEquality =
  chainl1
    pRelational
    ( choice
        [ symbol "==" $> EBinary Eq,
          symbol "!=" $> EBinary Neq
        ]
    )

-- | Parse relational expressions
pRelational :: Parser LExpr
pRelational =
  chainl1
    pAdditive
    ( choice
        [ symbol "<=" $> EBinary Lte,
          symbol "<" $> EBinary Lt,
          symbol ">=" $> EBinary Gte,
          symbol ">" $> EBinary Gt
        ]
    )

-- | Parse Additive binary operation:
--
-- * @+@
--
-- * @-@
pAdditive :: Parser LExpr
pAdditive =
  chainl1
    pMultiplicative
    ( choice
        [ symbol "+" $> EBinary Add,
          symbol "-" $> EBinary Sub
        ]
    )

-- | Parse multiplcative Binary operation:
--
-- * @*@
--
-- * @/@
--
-- * @%@
pMultiplicative :: Parser LExpr
pMultiplicative =
  chainl1
    pUnary
    ( choice
        [ symbol "*" $> EBinary Mul,
          symbol "/" $> EBinary Div,
          symbol "%" $> EBinary Mod
        ]
    )

-- | Parse Unary operation:
--
-- * @!@
--
-- * @-@
pUnary :: Parser LExpr
pUnary =
  choice
    [ withSpan $ EUnary Not <$> (symbol "!" *> pUnary),
      withSpan $ EUnary Neg <$> (symbol "-" *> pUnary),
      pPostfix
    ]

pPostfix :: Parser LExpr
pPostfix = do
  base <- pPrimary
  pChain base
  where
    pChain e =
      choice
        [ try $ do
            args <- parens (map Arg <$> (pExpr `sepBy` comma))
            endP <- getSourcePos
            let sp = Span (spanStart (locSpan e)) (toPos endP)
            pChain (Located sp (ECall e args)),
          try $ do
            void (symbol ".")
            f <- identifier
            endP <- getSourcePos
            let sp = Span (spanStart (locSpan e)) (toPos endP)
            pChain (Located sp (EMember e f)),
          try $ do
            ix <- brackets pExpr
            endP <- getSourcePos
            let sp = Span (spanStart (locSpan e)) (toPos endP)
            pChain (Located sp (EIndex e ix)),
          pure e
        ]

pPrimary :: Parser LExpr
pPrimary =
  choice
    [ try (withSpan pLambdaRaw),
      try (withSpan pIfExprRaw),
      try (withSpan pMatchExprRaw),
      withSpan (ELit <$> pLiteral),
      try (withSpan pObjectRaw),
      try (withSpan pArrayRaw),
      try (withSpan pVariantExprRaw),
      withSpan (EVar <$> identifier),
      withSpan (EParens <$> parens pExpr)
    ]

-- | Parse Lambda Expression (returns raw Expr, wrapped by withSpan)
pLambdaRaw :: Parser Expr
pLambdaRaw = do
  params <- parens (pParam `sepBy` comma)
  retTy <- optional (colon *> pType)
  void arrow
  body <- pExpr
  pure (ELam params retTy body)

-- | Parse if expression (returns raw Expr)
pIfExprRaw :: Parser Expr
pIfExprRaw = do
  rword "if"
  c <- parens pExpr
  t <- pExpr
  rword "else"
  f <- pExpr
  pure (EIfExpr c t f)

-- | Parse match expression (returns raw Expr).
-- @match (scrutinee) { Pattern => expr, ... }@
pMatchExprRaw :: Parser Expr
pMatchExprRaw = do
  rword "match"
  scrut <- parens pExpr
  arms <- braces (pMatchArm `MP.sepEndBy` comma)
  pure (EMatch scrut arms)
  where
    pMatchArm = do
      pat <- pPattern
      void arrow
      body <- pExpr
      pure (MatchArm pat body)

-- | Parse a pattern: @EnumName::VariantName(x, y)@ or @_@.
pPattern :: Parser Pattern
pPattern =
  choice
    [ try pVariantPat,
      symbol "_" $> PWild
    ]
  where
    pVariantPat = do
      enumName <- upperIdentifier
      void (symbol "::")
      varName <- upperIdentifier
      bindings <- MP.option [] (parens (identifier `sepBy` comma))
      pure (PVariant enumName varName bindings)

-- | Parse variant constructor expression (returns raw Expr).
-- @EnumName::VariantName(args)@ or @EnumName::VariantName@ (unit variant).
pVariantExprRaw :: Parser Expr
pVariantExprRaw = do
  enumName <- upperIdentifier
  void (symbol "::")
  varName <- upperIdentifier
  args <- MP.option [] (try $ parens (pExpr `sepBy` comma))
  pure (EVariant enumName varName args)

-- | Parse Literal:
--
-- * true
--
-- * false
--
-- * null
--
-- * String literals
--
-- * Integer literals
pLiteral :: Parser Literal
pLiteral =
  choice
    [ rword "true" $> LBool True,
      rword "false" $> LBool False,
      rword "null" $> LNull,
      LString <$> stringLit,
      LInt <$> integer
    ]

-- | Parse Arrays (returns raw Expr)
pArrayRaw :: Parser Expr
pArrayRaw = EArray <$> brackets (pExpr `MP.sepEndBy` comma)

-- | Parse Objects (returns raw Expr)
pObjectRaw :: Parser Expr
pObjectRaw = EObject <$> braces (pField `MP.sepEndBy` comma)
  where
    pField = do
      k <- identifier <|> stringLit
      void colon
      v <- pExpr
      pure (k, v)

pType :: Parser Type
pType = pFunType

-- Right-assoc function type:
-- (A, B) -> C
-- A -> B -> C   as A -> (B -> C)
pFunType :: Parser Type
pFunType = do
  lhs <- pTypeAtomOrTuple
  rest lhs
  where
    rest lhs =
      ( do
          void (symbol "->")
          rhs <- pFunType
          case lhs of
            TFun as r -> pure (TFun as (collapse rhs r))
            _ -> pure (TFun [lhs] rhs)
      )
        <|> pure lhs

    collapse r1 r2 = TFun [r2] r1

pTypeAtomOrTuple :: Parser Type
pTypeAtomOrTuple =
  choice
    [ parens $ do
        ts <- pType `sepBy` comma
        case ts of
          [] -> fail "empty type tuple not allowed"
          [t] -> pure t
          xs -> pure (TFun xs (TVar "_Tuple")), -- simple placeholder strategy
      pTypeAtom
    ]

pTypeAtom :: Parser Type
pTypeAtom =
  choice
    [ rword "Int" $> TInt,
      rword "Bool" $> TBool,
      rword "String" $> TString,
      try pArrayType,
      try pObjectType,
      try pTypeApp,
      try pTypeRef,
      TVar <$> identifier
    ]

-- | Parse a bare uppercase type reference (no angle brackets).
-- E.g. @Point@ becomes @TApp "Point" []@.
pTypeRef :: Parser Type
pTypeRef = do
  name <- upperIdentifier
  pure (TApp name [])

-- | Parse Array type declaration
pArrayType :: Parser Type
pArrayType = TArray <$> brackets pType

-- | Parse Object type declaration
pObjectType :: Parser Type
pObjectType = TObject <$> braces (pField `sepBy` comma)
  where
    pField = do
      k <- identifier <|> stringLit
      void colon
      t <- pType
      pure (k, t)

pTypeApp :: Parser Type
pTypeApp = do
  ctor <- identifier
  args <- angles (pType `sepBy1` comma)
  pure (TApp ctor args)
  where
    angles = between (symbol "<") (symbol ">")

-- | Left-associative binary operator chain that produces 'LExpr' nodes.
-- The operator function combines two 'LExpr' into a raw 'Expr', and the
-- chain automatically wraps the result in 'Located' with a span covering
-- both operands.
chainl1 :: Parser LExpr -> Parser (LExpr -> LExpr -> Expr) -> Parser LExpr
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          let sp = Span (spanStart (locSpan x)) (spanEnd (locSpan y))
          rest (Located sp (f x y))
      )
        <|> pure x
