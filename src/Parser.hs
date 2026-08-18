{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser
-- Description : Hand-written recursive-descent parser and AST generation for Fractum.
-- Stability   : experimental
--
-- This module implements the Fractum grammar as a hand-written recursive
-- descent parser over the token stream produced by "Parser.Lexer". It replaces
-- an earlier @megaparsec@ grammar; the surface AST it produces ('Program',
-- 'LExpr', 'LStmt') is unchanged.
--
-- == Architecture Overview
--
-- * __Separate lexing:__
--     "Parser.Lexer" runs first and turns the whole file into tokens. Because
--     every token knows its own span, an AST node's span now stops at the last
--     character of the node rather than after the trailing whitespace a lexeme
--     combinator would have eaten, so caret underlines cover exactly the
--     offending text.
--
-- * __LL(1) dispatch instead of backtracking:__
--     Statements, primary expressions and types are all chosen by looking at a
--     single token ('peek'). The one genuinely ambiguous construct — @(@
--     starting either a lambda's parameter list or a parenthesised expression —
--     is settled by 'lambdaAhead', a bounded scan for the matching @)@. No part
--     of the grammar backtracks over a partially consumed construct, which is
--     what allows an error to be reported at the exact token that caused it
--     rather than at the start of the longest failing alternative.
--
-- * __Pratt parsing for expressions:__
--     Rather than one function per precedence level, 'pExprBp' climbs
--     precedence directly from the 'infixOp' table. Left recursion, precedence
--     and associativity all fall out of the binding powers, and adding an
--     operator is one table entry. Prefix @!@ / @-@ and the postfix chain
--     (calls, @.field@, @[index]@) bind tighter than every infix operator and
--     are handled in 'pUnary' and 'pPostfix'.
--
-- * __Panic-mode recovery:__
--     Every statement is parsed at a recovery point (see 'Parser.Monad.recover').
--     A statement that fails records its diagnostic, is abandoned, and the
--     parser resynchronises on the next statement boundary and carries on, so a
--     single run reports as many independent syntax errors as the file contains.
--     'parseProgram' only returns a 'Program' when nothing at all went wrong.
--
-- == Entry Point
--
-- * 'parseProgram': lexes and parses a whole file, returning either every
--     diagnostic collected during the pass or the finished 'Program'.
module Parser
  ( parseProgram,
  )
where

import Ast
import Control.Monad (unless, void, when)
import Control.Monad.State.Strict (gets)
import Data.Functor (($>))
import Data.Text (Text)
import Diagnostic.Types (Note (..))
import Parser.Error (ParseError (..), ParseErrorKind (..))
import Parser.Lexer (lexTokens)
import Parser.Monad
import Parser.Token

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Lex and parse a complete source file.
--
-- Returns every diagnostic the pass collected, not just the first: a file with
-- three unrelated syntax errors reports three.
parseProgram :: FilePath -> Text -> Either [ParseError] Program
parseProgram file src =
  case runParser file toks lexErrs pProgram of
    (Just prog, []) -> Right prog
    (_, []) -> Left [panickedWithoutDiagnostic file]
    (_, errs) -> Left errs
  where
    (toks, lexErrs) = lexTokens file src

-- | Unreachable in practice: 'pProgram' places a recovery point around every
-- statement, so a panic always leaves a recorded diagnostic behind. Kept so
-- that 'parseProgram' is total rather than partial on an impossible case.
panickedWithoutDiagnostic :: FilePath -> ParseError
panickedWithoutDiagnostic file =
  ParseError
    (Span file (Pos 1 1) (Pos 1 1))
    (UnexpectedToken ["a declaration or statement"] "end of file")
    []

--------------------------------------------------------------------------------
-- Shared shapes
--
-- Almost every construct in the language is "a delimiter, some things joined
-- by commas, a delimiter". Expressing that once keeps the span arithmetic and
-- the error wording identical everywhere it appears.
--------------------------------------------------------------------------------

-- | Whether a separated list may end with a trailing separator.
data TrailingSep
  = NoTrailingSep
  | AllowTrailingSep
  deriving (Eq, Show)

-- | @open body close@, annotated with the span covering both delimiters.
enclosed :: Context -> Punct -> Punct -> Parser a -> Parser (Located a)
enclosed ctx open close body = do
  opener <- expect ctx (TPunct open)
  x <- body
  closer <- expectClose ctx open close [] (tokSpan opener)
  pure (Located (spanning (tokSpan opener) (tokSpan closer)) x)

-- | Consume a group's closing delimiter, or explain precisely what is missing.
--
-- Hitting end of file means the delimiter was never closed, and the useful
-- thing to point at is where it opened. Hitting some other token means the
-- group's contents ended earlier than expected, and the useful thing to say is
-- what could have continued it.
expectClose :: Context -> Punct -> Punct -> [Text] -> Span -> Parser Token
expectClose ctx open close alsoValid openSpan = do
  t <- peek
  case tokKind t of
    TPunct p | p == close -> advance
    TEof ->
      parseErrorWith
        ctx
        (UnclosedDelimiter (punctText open) (punctText close))
        [NoteSpan openSpan ("the `" <> punctText open <> "` opened here is never closed")]
    other ->
      parseErrorWith
        ctx
        (UnexpectedToken (("`" <> punctText close <> "`") : alsoValid) (describeKind other))
        [NoteSpan openSpan ("inside the `" <> punctText open <> "` opened here")]

-- | @open p sep p sep … close@, annotated with the span covering both
-- delimiters.
--
-- This is also the parser's second recovery point. A group that fails part way
-- through is skipped to its own closing delimiter and reported as empty, so
-- the caller resumes exactly where it would have anyway: a function whose
-- parameter list is malformed still has its body parsed, and any errors in
-- that body are still reported in the same pass. Only when the closing
-- delimiter cannot be found — the group runs to end of file, or into a bracket
-- belonging to an enclosing construct — is the panic passed further up.
delimited ::
  Context -> Punct -> Punct -> Punct -> TrailingSep -> Parser a -> Parser (Located [a])
delimited ctx open close sep trailing p = do
  opener <- expect ctx (TPunct open)
  outcome <- recover $ do
    xs <- sepList close sep trailing p
    closer <- expectClose ctx open close ["`" <> punctText sep <> "`"] (tokSpan opener)
    pure (xs, tokSpan closer)
  case outcome of
    Just (xs, closeSpan) -> pure (Located (spanning (tokSpan opener) closeSpan) xs)
    Nothing -> do
      closed <- skipToClose close
      unless closed panic
      end <- gets psPrevEnd
      pure (Located (tokSpan opener) {spanEnd = end} [])

-- | The contents of a 'delimited' group, stopping before the closing token.
sepList :: Punct -> Punct -> TrailingSep -> Parser a -> Parser [a]
sepList close sep trailing p = do
  isEmpty <- check (TPunct close)
  if isEmpty then pure [] else go
  where
    go = do
      x <- p
      more <- match (TPunct sep)
      if not more
        then pure [x]
        else case trailing of
          NoTrailingSep -> (x :) <$> go
          AllowTrailingSep -> do
            done <- check (TPunct close)
            if done then pure [x] else (x :) <$> go

-- | A value wrapped in parentheses, as conditions are.
parenthesised :: Context -> Parser a -> Parser a
parenthesised ctx p = locVal <$> enclosed ctx LParen RParen p

-- | An optional @: T@ annotation.
pTypeAnnotation :: Context -> Parser (Maybe Type)
pTypeAnnotation ctx = do
  annotated <- match (TPunct Colon)
  if annotated then Just <$> pType ctx else pure Nothing

-- | An optional @\<A, B\>@ type parameter list.
pTypeParams :: Context -> Parser [Name]
pTypeParams ctx = do
  present <- check (TPunct Less)
  if not present
    then pure []
    else locVal <$> delimited ctx Less Greater Comma NoTrailingSep (locVal <$> expectIdent ctx)

-- | A field name, which may be written bare or quoted.
pFieldName :: Context -> Parser Name
pFieldName ctx = do
  t <- peek
  case tokKind t of
    TIdent n -> advance $> n
    TStrLit s -> advance $> s
    other -> parseError ctx (UnexpectedToken ["a field name"] (describeKind other))

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

pProgram :: Parser Program
pProgram = Program <$> stmtsUntil TopLevel

-- | Parse statements until the end of the enclosing region, recovering from
-- any that fail.
--
-- This is the loop that makes multiple-error reporting work. A statement that
-- panics costs exactly one diagnostic: it is dropped, the stream is
-- resynchronised, and the next statement is parsed as if nothing had happened.
-- The 'psConsumed' comparison guarantees forward progress even when both the
-- failed statement and the resynchronisation consumed nothing.
--
-- 'Nesting' says where the region ends, and is handed to 'synchronize' so that
-- panic mode treats a @}@ the same way the loop itself does.
stmtsUntil :: Nesting -> Parser [LStmt]
stmtsUntil nesting = go
  where
    go = do
      done <- atEnd
      if done then pure [] else step

    atEnd = case nesting of
      TopLevel -> atEof
      InsideBlock -> (||) <$> check (TPunct RBrace) <*> atEof

    step = do
      before <- gets psConsumed
      outcome <- recover pStmt
      case outcome of
        Just s -> (s :) <$> go
        Nothing -> do
          synchronize nesting
          after <- gets psConsumed
          eof <- atEof
          when (after == before && not eof) (void advance)
          go

-- | A single statement, chosen from the leading token alone.
pStmt :: Parser LStmt
pStmt =
  peekKind >>= \case
    TKw KwEnum -> withSpan pEnumDecl
    TKw KwType -> withSpan pTypeDecl
    TKw KwFunction -> withSpan pFunDecl
    TKw KwLet -> withSpan pLet
    TKw KwImport -> withSpan pImportDecl
    TKw KwExport -> withSpan pExportDecl
    TKw KwReturn -> withSpan pReturn
    TKw KwIf -> withSpan pIfStmt
    TKw KwWhile -> withSpan pWhile
    TPunct LBrace -> withSpan (SBlock <$> pBlock (Context "a block"))
    _ -> withSpan pExprStmt

-- | A braced statement sequence.
--
-- The inner statement loop stops at @}@ /or/ at end of file, so an unclosed
-- brace surfaces as one "unclosed delimiter" error pointing at the offending
-- @{@ rather than as a cascade from the rest of the file.
pBlock :: Context -> Parser Block
pBlock ctx = do
  opener <- expect ctx (TPunct LBrace)
  stmts <- stmtsUntil InsideBlock
  _ <- expectClose ctx LBrace RBrace [] (tokSpan opener)
  pure (Block stmts)

-- | @let x = e;@, @let mut x: T = e;@
pLet :: Parser Stmt
pLet = do
  _ <- expect ctx (TKw KwLet)
  mutable <- match (TKw KwMut)
  Located _ name <- expectIdent ctx
  ty <- pTypeAnnotation ctx
  _ <- expect ctx (TPunct Assign)
  value <- pExpr
  _ <- expectSemi ctx
  pure (SLet (if mutable then Mutable else Immutable) name ty value)
  where
    ctx = Context "a `let` declaration"

-- | @type Point = { x: Int, y: Int };@, including parametric aliases such as
-- @type Pair\<A, B\> = { first: A, second: B };@
pTypeDecl :: Parser Stmt
pTypeDecl = do
  _ <- expect ctx (TKw KwType)
  Located _ name <- expectUpperIdent ctx
  params <- pTypeParams ctx
  _ <- expect ctx (TPunct Assign)
  body <- pType ctx
  _ <- expectSemi ctx
  pure (STypeDecl name params body)
  where
    ctx = Context "a type declaration"

-- | @enum Shape { Circle(Int), Point }@, including parametric enums such as
-- @enum Option\<T\> { Some(T), None }@
pEnumDecl :: Parser Stmt
pEnumDecl = do
  _ <- expect ctx (TKw KwEnum)
  Located _ name <- expectUpperIdent ctx
  params <- pTypeParams ctx
  variants <- delimited ctx LBrace RBrace Comma AllowTrailingSep pVariant
  pure (SEnum name params (locVal variants))
  where
    ctx = Context "an enum declaration"
    pVariant = do
      Located _ name <- expectUpperIdent ctx
      present <- check (TPunct LParen)
      fields <-
        if present
          then locVal <$> delimited ctx LParen RParen Comma NoTrailingSep (pType ctx)
          else pure []
      pure (Variant name fields)

-- | @import { a, b as c } from "./path";@
pImportDecl :: Parser Stmt
pImportDecl = do
  _ <- expect ctx (TKw KwImport)
  items <- delimited ctx LBrace RBrace Comma NoTrailingSep pImportItem
  _ <- expectWith ctx [NoteHelp "an import list is followed by `from \"./module\"`"] (TKw KwFrom)
  Located _ path <- expectString ctx
  _ <- expectSemi ctx
  pure (SImport (locVal items) path)
  where
    ctx = Context "an import declaration"
    pImportItem = do
      Located _ name <- expectIdent ctx
      aliased <- match (TKw KwAs)
      alias <- if aliased then Just . locVal <$> expectIdent ctx else pure Nothing
      pure (ImportItem name alias)

-- | @export \<let|function|type|enum\>@
pExportDecl :: Parser Stmt
pExportDecl = do
  _ <- expect ctx (TKw KwExport)
  SExport <$> withSpan pExported
  where
    ctx = Context "an export declaration"
    pExported =
      peekKind >>= \case
        TKw KwLet -> pLet
        TKw KwFunction -> pFunDecl
        TKw KwType -> pTypeDecl
        TKw KwEnum -> pEnumDecl
        other ->
          parseErrorWith
            ctx
            (UnexpectedToken ["`let`", "`function`", "`type`", "`enum`"] (describeKind other))
            [NoteHelp "only `let`, `function`, `type` and `enum` declarations can be exported"]

-- | @function name\<T\>(a: T): R { … }@
pFunDecl :: Parser Stmt
pFunDecl = do
  _ <- expect ctx (TKw KwFunction)
  Located _ name <- expectIdent ctx
  tyParams <- pTypeParams ctx
  params <- delimited ctx LParen RParen Comma NoTrailingSep (pParam ctx)
  retTy <- pTypeAnnotation ctx
  body <- pBlock ctx
  pure (SFun name tyParams (locVal params) retTy body)
  where
    ctx = Context "a function declaration"

-- | @return;@ or @return e;@
pReturn :: Parser Stmt
pReturn = do
  _ <- expect ctx (TKw KwReturn)
  bare <- check (TPunct Semi)
  value <- if bare then pure Nothing else Just <$> pExpr
  _ <- expectSemi ctx
  pure (SReturn value)
  where
    ctx = Context "a `return` statement"

-- | @if (cond) { … } else { … }@. The branches of an @if@ /statement/ are
-- always blocks; @if@ used as an expression is 'pIfExpr'.
pIfStmt :: Parser Stmt
pIfStmt = do
  _ <- expect ctx (TKw KwIf)
  cond <- parenthesised ctx pExpr
  thenB <- pBlock ctx
  hasElse <- match (TKw KwElse)
  elseB <- if hasElse then Just <$> pBlock ctx else pure Nothing
  pure (SIf cond thenB elseB)
  where
    ctx = Context "an `if` statement"

-- | @while (cond) { … }@
pWhile :: Parser Stmt
pWhile = do
  _ <- expect ctx (TKw KwWhile)
  cond <- parenthesised ctx pExpr
  body <- pBlock ctx
  pure (SWhile cond body)
  where
    ctx = Context "a `while` loop"

-- | A bare expression terminated by @;@.
pExprStmt :: Parser Stmt
pExprStmt = do
  e <- pExpr
  _ <- expectSemi ctx
  pure (SExpr e)
  where
    ctx = Context "an expression statement"

-- | A function or lambda parameter.
pParam :: Context -> Parser Param
pParam ctx = do
  Located _ name <- expectIdent ctx
  Param name <$> pTypeAnnotation ctx

-- | The statement terminator, with a nudge when it is the thing missing.
expectSemi :: Context -> Parser Token
expectSemi ctx = expectWith ctx [NoteHelp "statements are terminated with `;`"] (TPunct Semi)

--------------------------------------------------------------------------------
-- Expressions: Pratt parser
--
-- Precedence and associativity live entirely in 'infixOp'. 'pExprBp' consumes
-- an operator only while its precedence is at least the caller's minimum,
-- which is what turns the flat token stream into a correctly nested tree
-- without a function per precedence level.
--------------------------------------------------------------------------------

data Assoc = AssocLeft | AssocRight
  deriving (Eq, Show)

-- | The node an infix operator builds. Kept as data rather than a function so
-- that the operator table stays inspectable and comparable.
data InfixNode
  = NodeAssign
  | NodeBinary !BinOp
  deriving (Eq, Show)

data InfixOp = InfixOp
  { opPrec :: !Int,
    opAssoc :: !Assoc,
    opNode :: !InfixNode
  }
  deriving (Eq, Show)

-- | The infix operator table: the whole of Fractum's precedence and
-- associativity, in one place.
--
-- Assignment is lowest and right-associative, so @a = b = c@ groups as
-- @a = (b = c)@ and its left operand is a full postfix expression, which is
-- what lets @obj.field = v@ and @arr[i] = v@ parse without a special case.
infixOp :: TokenKind -> Maybe InfixOp
infixOp = \case
  TPunct Assign -> Just (InfixOp 1 AssocRight NodeAssign)
  TPunct PipePipe -> Just (InfixOp 2 AssocLeft (NodeBinary Or))
  TPunct AmpAmp -> Just (InfixOp 3 AssocLeft (NodeBinary And))
  TPunct EqEq -> Just (InfixOp 4 AssocLeft (NodeBinary Eq))
  TPunct BangEq -> Just (InfixOp 4 AssocLeft (NodeBinary Neq))
  TPunct LessEq -> Just (InfixOp 5 AssocLeft (NodeBinary Lte))
  TPunct Less -> Just (InfixOp 5 AssocLeft (NodeBinary Lt))
  TPunct GreaterEq -> Just (InfixOp 5 AssocLeft (NodeBinary Gte))
  TPunct Greater -> Just (InfixOp 5 AssocLeft (NodeBinary Gt))
  TPunct Plus -> Just (InfixOp 6 AssocLeft (NodeBinary Add))
  TPunct Minus -> Just (InfixOp 6 AssocLeft (NodeBinary Sub))
  TPunct Star -> Just (InfixOp 7 AssocLeft (NodeBinary Mul))
  TPunct Slash -> Just (InfixOp 7 AssocLeft (NodeBinary Div))
  TPunct Percent -> Just (InfixOp 7 AssocLeft (NodeBinary Mod))
  _ -> Nothing

-- | The minimum precedence the right operand must clear. Bumping it by one for
-- left-associative operators is what stops @a - b - c@ from grouping to the
-- right.
rightBindingPower :: InfixOp -> Int
rightBindingPower op = case opAssoc op of
  AssocLeft -> opPrec op + 1
  AssocRight -> opPrec op

buildInfix :: InfixNode -> LExpr -> LExpr -> Expr
buildInfix NodeAssign = EAssign
buildInfix (NodeBinary op) = EBinary op

pExpr :: Parser LExpr
pExpr = pExprBp 0

-- | Parse an expression, folding in every infix operator that binds at least
-- as tightly as @minBp@.
pExprBp :: Int -> Parser LExpr
pExprBp minBp = pCast >>= loop
  where
    loop lhs =
      peekKind >>= \k -> case infixOp k of
        Just op | opPrec op >= minBp -> do
          _ <- advance
          rhs <- pExprBp (rightBindingPower op)
          let node = buildInfix (opNode op) lhs rhs
          loop (Located (spanning (locSpan lhs) (locSpan rhs)) node)
        _ -> pure lhs

-- | @e as T@, binding tighter than every infix operator but looser than
-- unary prefix and the postfix chain, so @x + y as Float@ is @x + (y as
-- Float)@ and @-x as Float@ is @(-x) as Float@. Right-associative by simply
-- looping: @x as Int as Float@ reads as @(x as Int) as Float@.
pCast :: Parser LExpr
pCast = pUnary >>= loop
  where
    loop e = do
      isAs <- match (TKw KwAs)
      if not isAs
        then pure e
        else do
          ty <- pTypeAtom ctx
          end <- gets psPrevEnd
          loop (Located (locSpan e) {spanEnd = end} (ECast e ty))
    ctx = Context "a type cast"

-- | Prefix @!@ and @-@, which bind tighter than every infix operator: @-a * b@
-- is @(-a) * b@ and @-f(x)@ is @-(f(x))@.
pUnary :: Parser LExpr
pUnary =
  peekKind >>= \case
    TPunct Bang -> prefix Not
    TPunct Minus -> prefix Neg
    _ -> pPostfix
  where
    prefix op = withSpan (EUnary op <$> (advance *> pUnary))

-- | The postfix chain: calls, member access and indexing, applied left to
-- right to a primary expression.
--
-- Each step extends the span from the start of the base expression to the end
-- of the postfix operation just consumed, so @a.b(c)[d]@ reports as one node
-- covering all of it.
pPostfix :: Parser LExpr
pPostfix = pPrimary >>= chain
  where
    chain e =
      peekKind >>= \case
        TPunct LParen -> do
          args <- delimited ctx LParen RParen Comma NoTrailingSep pExpr
          extend e (locSpan args) (ECall e (map Arg (locVal args)))
        TPunct Dot -> do
          _ <- advance
          field <- expectIdent ctx
          extend e (locSpan field) (EMember e (locVal field))
        TPunct LBracket -> do
          index <- enclosed ctx LBracket RBracket pExpr
          extend e (locSpan index) (EIndex e (locVal index))
        _ -> pure e

    -- Grow the node's span from the start of the base expression to the end of
    -- the postfix operation just consumed, then keep chaining.
    extend base end node = chain (Located (spanning (locSpan base) end) node)

    ctx = Context "an expression"

-- | An atomic expression, chosen from the leading token alone.
pPrimary :: Parser LExpr
pPrimary =
  peekKind >>= \case
    TKw KwTrue -> literal (LBool True)
    TKw KwFalse -> literal (LBool False)
    TKw KwNull -> literal LNull
    TIntLit n -> literal (LInt n)
    TFloatLit n -> literal (LFloat n)
    TStrLit s -> literal (LString s)
    TKw KwIf -> withSpan pIfExpr
    TKw KwMatch -> withSpan pMatchExpr
    TPunct LBracket -> withSpan pArrayLit
    TPunct LBrace -> withSpan pObjectLit
    TPunct LParen -> pParensOrLambda
    TIdent n -> withSpan (pVarOrVariant n)
    other ->
      parseErrorWith
        ctx
        (UnexpectedToken ["an expression"] (describeKind other))
        [NoteText "an expression is a literal, a name, or one of `(`, `[`, `{`, `if`, `match`"]
  where
    literal l = withSpan (advance $> ELit l)
    ctx = Context "an expression"

-- | @(a, b) => e@ and @(a): T => e@ against @(e)@.
--
-- Both start with @(@; 'lambdaAhead' decides which by looking past the
-- matching @)@, so neither alternative is ever parsed and thrown away.
pParensOrLambda :: Parser LExpr
pParensOrLambda = do
  isLambda <- lambdaAhead
  if isLambda
    then withSpan pLambda
    else withSpan (EParens . locVal <$> enclosed ctx LParen RParen pExpr)
  where
    ctx = Context "a parenthesised expression"

pLambda :: Parser Expr
pLambda = do
  params <- delimited ctx LParen RParen Comma NoTrailingSep (pParam ctx)
  retTy <- pTypeAnnotation ctx
  _ <- expect ctx (TPunct FatArrow)
  ELam (locVal params) retTy <$> pExpr
  where
    ctx = Context "a lambda"

-- | A bare name, or a qualified variant constructor @Enum::Variant(args)@.
pVarOrVariant :: Name -> Parser Expr
pVarOrVariant name = do
  _ <- advance
  qualified <- check (TPunct ColonColon)
  if not (qualified && isUpperName name)
    then pure (EVar name)
    else do
      _ <- advance
      Located _ variant <- expectUpperIdent ctx
      present <- check (TPunct LParen)
      args <-
        if present
          then locVal <$> delimited ctx LParen RParen Comma NoTrailingSep pExpr
          else pure []
      pure (EVariant name variant args)
  where
    ctx = Context "an enum variant"

-- | @if (c) a else b@ as an expression; both branches are expressions, and the
-- @else@ is mandatory because the expression must have a value.
pIfExpr :: Parser Expr
pIfExpr = do
  _ <- expect ctx (TKw KwIf)
  cond <- parenthesised ctx pExpr
  thenE <- pExpr
  _ <- expectWith ctx [NoteHelp "an `if` expression must have an `else` branch, since it has to produce a value"] (TKw KwElse)
  EIfExpr cond thenE <$> pExpr
  where
    ctx = Context "an `if` expression"

-- | @match (scrutinee) { Pattern => expr, … }@
pMatchExpr :: Parser Expr
pMatchExpr = do
  _ <- expect ctx (TKw KwMatch)
  scrutinee <- parenthesised ctx pExpr
  arms <- delimited ctx LBrace RBrace Comma AllowTrailingSep pArm
  pure (EMatch scrutinee (locVal arms))
  where
    ctx = Context "a `match` expression"
    pArm = do
      pat <- pPattern ctx
      _ <- expect ctx (TPunct FatArrow)
      MatchArm pat <$> pExpr

-- | @Enum::Variant(x, y)@ or the wildcard @_@. Patterns are flat: a binding is
-- a plain name, never a nested pattern.
pPattern :: Context -> Parser Pattern
pPattern ctx =
  peekKind >>= \case
    TIdent "_" -> advance $> PWild
    TIdent name | isUpperName name -> pVariantPat name
    other ->
      parseErrorWith
        ctx
        (UnexpectedToken ["a pattern"] (describeKind other))
        [NoteHelp "a pattern is either `Enum::Variant(x, y)` or the wildcard `_`"]
  where
    pVariantPat name = do
      _ <- advance
      _ <- expect ctx (TPunct ColonColon)
      Located _ variant <- expectUpperIdent ctx
      present <- check (TPunct LParen)
      bindings <-
        if present
          then locVal <$> delimited ctx LParen RParen Comma NoTrailingSep (locVal <$> expectIdent ctx)
          else pure []
      pure (PVariant name variant bindings)

pArrayLit :: Parser Expr
pArrayLit =
  EArray . locVal <$> delimited ctx LBracket RBracket Comma AllowTrailingSep pExpr
  where
    ctx = Context "an array literal"

pObjectLit :: Parser Expr
pObjectLit =
  EObject . locVal <$> delimited ctx LBrace RBrace Comma AllowTrailingSep pField
  where
    ctx = Context "an object literal"
    pField = do
      key <- pFieldName ctx
      _ <- expect ctx (TPunct Colon)
      value <- pExpr
      pure (key, value)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | What sits to the left of a possible @->@.
--
-- A parenthesised group is genuinely ambiguous until the @->@ either shows up
-- or does not: @(Int)@ is just @Int@, while @(Int, Int)@ is only meaningful as
-- a parameter list. Modelling that as data rather than guessing is what lets
-- @(A, B) -> C@ parse as the two-parameter function type it obviously is.
data TypeLhs
  = LhsType Type
  | LhsParams [Type]
  deriving (Eq, Show)

-- | A type, right-associative in @->@: @A -> B -> C@ is @A -> (B -> C)@.
pType :: Context -> Parser Type
pType ctx = do
  lhs <- pTypeLhs
  isFun <- match (TPunct Arrow)
  case (lhs, isFun) of
    (LhsType t, False) -> pure t
    (LhsType t, True) -> TFun [t] <$> pType ctx
    (LhsParams ts, True) -> TFun ts <$> pType ctx
    (LhsParams [t], False) -> pure t
    (LhsParams _, False) -> parseError ctx DanglingParamList
  where
    pTypeLhs = do
      parenthesied <- check (TPunct LParen)
      if parenthesied
        then LhsParams . locVal <$> delimited ctx LParen RParen Comma NoTrailingSep (pType ctx)
        else LhsType <$> pTypeAtom ctx

-- | An atomic type: a builtin, an array, an object shape, or a named type with
-- optional type arguments.
--
-- A bare lowercase name is a type variable; a bare capitalised name is a
-- nullary application, which is what lets the checker resolve it against
-- whatever type parameters, aliases or enums are in scope.
pTypeAtom :: Context -> Parser Type
pTypeAtom ctx =
  peekKind >>= \case
    TKw KwInt -> advance $> TInt
    TKw KwFloat -> advance $> TFloat
    TKw KwBool -> advance $> TBool
    TKw KwString -> advance $> TString
    TPunct LBracket -> TArray . locVal <$> enclosed ctx LBracket RBracket (pType ctx)
    TPunct LBrace ->
      TObject . locVal <$> delimited ctx LBrace RBrace Comma NoTrailingSep pTypeField
    TIdent name -> advance *> pNamed name
    other -> parseError ctx (UnexpectedToken ["a type"] (describeKind other))
  where
    pNamed name = do
      applied <- check (TPunct Less)
      if applied
        then TApp name . locVal <$> delimited ctx Less Greater Comma NoTrailingSep (pType ctx)
        else pure (if isUpperName name then TApp name [] else TVar name)

    pTypeField = do
      key <- pFieldName ctx
      _ <- expect ctx (TPunct Colon)
      value <- pType ctx
      pure (key, value)
