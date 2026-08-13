{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Javascript Code generator. Outputs Readable Javascript.
module Emit 
  ( emitProgram
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified JsIr as J

emitProgram :: J.JSProgram -> Text
emitProgram (J.JSProgram ss) =
  T.intercalate "\n" (map (ppStmt 0) ss) <> "\n"

ppStmt :: Int -> J.JSStmt -> Text
ppStmt n = \case
  J.JSConst x e ->
    indent n <> "const " <> x <> " = " <> ppExpr 0 e <> ";"

  J.JSLet x e ->
    indent n <> "let " <> x <> " = " <> ppExpr 0 e <> ";"

  J.JSFunction name params blk ->
    indent n <> "function " <> name <> "(" <> csv params <> ") " <> ppBlock n blk

  J.JSReturn me ->
    indent n <> "return" <> maybe ";" (\e -> " " <> ppExpr 0 e <> ";") me

  J.JSIf c t me ->
    let headIf = indent n <> "if (" <> ppExpr 0 c <> ") " <> ppBlock n t
    in case me of
         Nothing -> headIf
         Just eb -> headIf <> " else " <> ppBlock n eb

  J.JSWhile c b ->
    indent n <> "while (" <> ppExpr 0 c <> ") " <> ppBlock n b

  J.JSExprStmt e ->
    indent n <> ppExpr 0 e <> ";"

  J.JSBlockStmt b ->
    indent n <> ppBlock n b

ppBlock :: Int -> J.JSBlock -> Text
ppBlock n (J.JSBlock ss) =
  "{\n"
  <> T.intercalate "\n" (map (ppStmt (n + 1)) ss)
  <> "\n" <> indent n <> "}"

-- Precedence: bigger = tighter binding
precAssign, precTernary, precOr, precAnd, precEq, precRel, precAdd, precMul, precUnary, precCall :: Int
precAssign  = 1
precTernary = 2
precOr      = 3
precAnd     = 4
precEq      = 5
precRel     = 6
precAdd     = 7
precMul     = 8
precUnary   = 9
precCall    = 10
-- precAtom    = 11

ppExpr :: Int -> J.JSExpr -> Text
ppExpr ctx = \case
  -- Special case for prelude print function which maps to Js's console.log
  J.JSVar "print" -> "console.log"

  -- Special case for prelude toString function which maps to Js's String constructor
  J.JSVar "toString" -> "String"

  J.JSVar x -> x
  
  J.JSLiteral l -> ppLit l

  J.JSArrow ps body ->
    parenIf (ctx > precAssign) $
      "(" <> csv ps <> ") => " <> ppExpr precAssign body

  J.JSCall f args ->
    parenIf (ctx > precCall) $
      ppExpr precCall f <> "(" <> csv (map (ppExpr 0) args) <> ")"

  J.JSMember o f ->
    parenIf (ctx > precCall) $
      ppExpr precCall o <> "." <> f

  J.JSIndex a i ->
    parenIf (ctx > precCall) $
      ppExpr precCall a <> "[" <> ppExpr 0 i <> "]"

  J.JSObject fs ->
    "{ " <> csv [k <> ": " <> ppExpr 0 v | (k,v) <- fs] <> " }"

  J.JSArray es ->
    "[" <> csv (map (ppExpr 0) es) <> "]"

  J.JSAssign l r ->
    parenIf (ctx > precAssign) $
      ppExpr precCall l <> " = " <> ppExpr precAssign r

  J.JSUnary op e ->
    parenIf (ctx > precUnary) $
      op <> ppExpr precUnary e

  J.JSBinary op a b ->
    let p = binPrec op
    in parenIf (ctx > p) $
         ppExpr p a <> " " <> op <> " " <> ppExpr (p + 1) b

  J.JSTernary c t f ->
    parenIf (ctx > precTernary) $
      ppExpr precOr c <> " ? " <> ppExpr precAssign t <> " : " <> ppExpr precAssign f

  J.JSParens e ->
    "(" <> ppExpr 0 e <> ")"

  J.JSIife blk ->
    "(() => " <> ppBlock ctx blk <> ")()"

binPrec :: Text -> Int
binPrec op
  | op == "||" = precOr
  | op == "&&" = precAnd
  | op == "==" || op == "!=" || op == "===" || op == "!==" = precEq
  | op == "<" || op == "<=" || op == ">" || op == ">=" = precRel
  | op == "+" || op == "-" = precAdd
  | op == "*" || op == "/" || op == "%" = precMul
  | otherwise = precAdd

ppLit :: J.JSLit -> Text
ppLit = \case
  J.JSInt n -> T.pack (show n)
  J.JSBool True -> "true"
  J.JSBool False -> "false"
  J.JSString s -> "\"" <> escape s <> "\""
  J.JSNull -> "null"

escape :: Text -> Text
escape = T.concatMap go
  where
    go '"'  = "\\\""
    go '\\' = "\\\\"
    go '\n' = "\\n"
    go '\t' = "\\t"
    go c    = T.singleton c

csv :: [Text] -> Text
csv = T.intercalate ", "

indent :: Int -> Text
indent n = T.replicate n "  "

parenIf :: Bool -> Text -> Text
parenIf True t  = "(" <> t <> ")"
parenIf False t = t
