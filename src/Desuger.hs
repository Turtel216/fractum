{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Responsible for lowering the Typed-AST into the Javascript IR.
module Desuger where

import Data.Text (Text)
import qualified Data.Text as T
import qualified JsIr as J
import Ast
  ( Program(..), Stmt(..), Expr(..), Literal(..), Param(..), Arg(..), Block(..)
  , BinOp(..), UnOp(..), Mutability(..), Located(..), LExpr, LStmt
  , MatchArm(..), Pattern(..), Type(..)
  )

-- | Type erasure + structural lowering from TypedJS AST to JS AST.
lowerProgram :: Program -> J.JSProgram
lowerProgram (Program ss) = J.JSProgram (map lowerStmt (filter (not . isCompileTimeDecl) ss))

-- | Type and enum declarations (and imports, fully consumed by the module
-- resolver before this stage) are compile-time only and produce no JS
-- output. An 'SExport' defers to whatever it wraps.
isCompileTimeDecl :: LStmt -> Bool
isCompileTimeDecl (Located _ (STypeDecl {}))  = True
isCompileTimeDecl (Located _ (SEnum {}))      = True
isCompileTimeDecl (Located _ (SImport {}))    = True
isCompileTimeDecl (Located _ (SExport inner)) = isCompileTimeDecl inner
isCompileTimeDecl _                           = False

-- | Lower LStmt into JsStmt
lowerStmt :: LStmt -> J.JSStmt
lowerStmt (Located _ stmt) = case stmt of
  SLet mut n _ e -> case mut of
    Immutable -> J.JSConst n (lowerExpr e)
    Mutable   -> J.JSLet   n (lowerExpr e)

  SFun name _tyParams params _retTy body ->
    J.JSFunction name (map paramName params) (lowerBlock body)

  SReturn me ->
    J.JSReturn (lowerExpr <$> me)

  SIf c t me ->
    J.JSIf (lowerExpr c) (lowerBlock t) (lowerBlock <$> me)

  SWhile c b ->
    J.JSWhile (lowerExpr c) (lowerBlock b)

  SExpr e ->
    J.JSExprStmt (lowerExpr e)

  SBlock b ->
    J.JSBlockStmt (lowerBlock b)

  STypeDecl {} ->
    error "lowerStmt: STypeDecl should have been filtered out"

  SEnum {} ->
    error "lowerStmt: SEnum should have been filtered out"

  SExport inner ->
    lowerStmt inner

  SImport {} ->
    error "lowerStmt: SImport should have been filtered out"

lowerBlock :: Block -> J.JSBlock
lowerBlock (Block ss) = J.JSBlock (map lowerStmt (filter (not . isCompileTimeDecl) ss))

-- | Lower LExpr into JSExpr, discarding the source span.
lowerExpr :: LExpr -> J.JSExpr
lowerExpr (Located _ expr) = case expr of
  EVar x -> J.JSVar x
  ELit l -> J.JSLiteral (lowerLit l)

  ELam params _retTy body ->
    J.JSArrow (map paramName params) (lowerExpr body)

  ECall f args ->
    J.JSCall (lowerExpr f) (map lowerArg args)

  EMember o f ->
    J.JSMember (lowerExpr o) f

  EIndex a i ->
    J.JSIndex (lowerExpr a) (lowerExpr i)

  EObject fs ->
    J.JSObject [ (k, lowerExpr v) | (k,v) <- fs ]

  EArray es ->
    J.JSArray (map lowerExpr es)

  EAssign l r ->
    J.JSAssign (lowerExpr l) (lowerExpr r)

  EUnary op e ->
    J.JSUnary (lowerUnOp op) (lowerExpr e)

  EBinary op a b ->
    J.JSBinary (lowerBinOp op) (lowerExpr a) (lowerExpr b)

  EIfExpr c t f ->
    J.JSTernary (lowerExpr c) (lowerExpr t) (lowerExpr f)

  EParens e ->
    J.JSParens (lowerExpr e)

  -- | Variant constructor: Shape::Circle(5) => { _tag: "Circle", _0: 5 }
  EVariant _enumName varName args ->
    let tagField = ("_tag", J.JSLiteral (J.JSString varName))
        dataFields = zipWith (\i a -> (indexField i, lowerExpr a)) [0 :: Int ..] args
    in J.JSObject (tagField : dataFields)

  -- | Match expression: compiled to an IIFE with if-else chain.
  -- match (x) { A::B(y) => e1, A::C => e2 }
  -- =>
  -- (() => {
  --   const _m = x;
  --   if (_m._tag === "B") { const y = _m._0; return e1; }
  --   if (_m._tag === "C") { return e2; }
  -- })()
  EMatch scrut arms ->
    let scrutJs = lowerExpr scrut
        bindScrut = J.JSConst "_m" scrutJs
        armStmts = concatMap lowerMatchArm arms
    in J.JSIife (J.JSBlock (bindScrut : armStmts))

  -- | JS has a single number type, so a cast is either a no-op (Int -> Float,
  -- or casting to the type an expression already has) or, going the other
  -- way, an explicit truncation towards zero to match the value an Int
  -- actually holds.
  ECast e TInt -> J.JSCall (J.JSMember (J.JSVar "Math") "trunc") [lowerExpr e]
  ECast e _    -> lowerExpr e

-- | Lower a single match arm to a list of JS statements.
-- Variant arms become: if (_m._tag === "Foo") { const x = _m._0; ... return body; }
-- Wildcard arms become: return body;
lowerMatchArm :: MatchArm -> [J.JSStmt]
lowerMatchArm (MatchArm pat body) = case pat of
  PWild ->
    [J.JSReturn (Just (lowerExpr body))]
  PVariant _enumName varName bindings ->
    let cond = J.JSBinary "==="
                 (J.JSMember (J.JSVar "_m") "_tag")
                 (J.JSLiteral (J.JSString varName))
        -- Bind each captured variable: const x = _m._0; const y = _m._1; ...
        bindStmts = zipWith (\i n ->
          J.JSConst n (J.JSMember (J.JSVar "_m") (indexField i))
          ) [0 :: Int ..] bindings
        retStmt = J.JSReturn (Just (lowerExpr body))
    in [J.JSIf cond (J.JSBlock (bindStmts ++ [retStmt])) Nothing]

-- | Generate field name for positional variant data: _0, _1, _2, ...
indexField :: Int -> Text
indexField i = "_" <> T.pack (show i)

lowerArg :: Arg -> J.JSExpr
lowerArg (Arg e) = lowerExpr e

lowerLit :: Literal -> J.JSLit
lowerLit = \case
  LInt n    -> J.JSInt n
  LFloat n  -> J.JSFloat n
  LBool b   -> J.JSBool b
  LString s -> J.JSString s
  LNull     -> J.JSNull

paramName :: Param -> Text
paramName (Param n _ty) = n

lowerUnOp :: UnOp -> Text
lowerUnOp = \case
  Neg -> "-"
  Not -> "!"

lowerBinOp :: BinOp -> Text
lowerBinOp = \case
  Mul -> "*"
  Div -> "/"
  Mod -> "%"
  Add -> "+"
  Sub -> "-"
  Lt  -> "<"
  Lte -> "<="
  Gt  -> ">"
  Gte -> ">="
  Eq  -> "=="
  Neq -> "!="
  And -> "&&"
  Or  -> "||"
