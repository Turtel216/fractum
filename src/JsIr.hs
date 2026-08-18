-- | Intermediete representation for untyped Javascript program. Used for Code emintion.
module JsIr where

import Data.Text (Text)

-- | Untyped javascript program IR
newtype JSProgram = JSProgram [JSStmt]
  deriving (Eq, Show)

-- | Untyped Javascript code block IR
newtype JSBlock = JSBlock [JSStmt]
  deriving (Eq, Show)

-- | Untyped Javscript statement IR
data JSStmt
  = JSConst Text JSExpr
  | JSLet Text JSExpr
  | JSFunction Text [Text] JSBlock
  | JSReturn (Maybe JSExpr)
  | JSIf JSExpr JSBlock (Maybe JSBlock)
  | JSWhile JSExpr JSBlock
  | JSExprStmt JSExpr
  | JSBlockStmt JSBlock
  deriving (Eq, Show)

-- | Untyped Javascript Expression IR
data JSExpr
  = JSVar Text
  | JSLiteral JSLit
  | JSArrow [Text] JSExpr
  | JSCall JSExpr [JSExpr]
  | JSMember JSExpr Text
  | JSIndex JSExpr JSExpr
  | JSObject [(Text, JSExpr)]
  | JSArray [JSExpr]
  | JSAssign JSExpr JSExpr
  | JSUnary Text JSExpr
  | JSBinary Text JSExpr JSExpr
  | JSTernary JSExpr JSExpr JSExpr
  | JSParens JSExpr
  | JSIife JSBlock           -- ^ (() => { ...stmts... })()
  deriving (Eq, Show)

-- | Untyped Javascript Literal IR
data JSLit
  = JSInt Integer
  | JSFloat Double
  | JSBool Bool
  | JSString Text
  | JSNull
  deriving (Eq, Show)
