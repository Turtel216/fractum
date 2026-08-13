-- | Abstract syntax tree definition
module Ast where

import Data.Text (Text)

type Name = Text

-- | Source position (1-indexed line and column)
data Pos = Pos { posLine :: !Int, posCol :: !Int }
  deriving (Eq, Ord, Show)

-- | Source span from start to end
data Span = Span { spanStart :: !Pos, spanEnd :: !Pos }
  deriving (Eq, Ord, Show)

-- | A dummy span for synthetic / generated nodes
dummySpan :: Span
dummySpan = Span (Pos 0 0) (Pos 0 0)

-- | A value annotated with a source span
data Located a = Located { locSpan :: !Span, locVal :: a }
  deriving (Eq, Show)

-- | Convenience aliases for located AST nodes
type LExpr = Located Expr
type LStmt = Located Stmt

-- | Binding mutability qualifier
data Mutability = Immutable | Mutable
  deriving (Eq, Show)

-- | Complete program definition
newtype Program = Program [LStmt]
  deriving (Eq, Show)

-- | Language Statement
data Stmt
  = SLet Mutability Name (Maybe Type) LExpr
  | SFun Name [Name] [Param] (Maybe Type) Block
  | SReturn (Maybe LExpr)
  | SIf LExpr Block (Maybe Block)
  | SWhile LExpr Block
  | SExpr LExpr
  | SBlock Block
  | STypeDecl Name [Name] Type   -- ^ e.g. type Foo<A, B> = { ... };
  | SEnum Name [Name] [Variant]  -- ^ e.g. enum Shape<T> { Circle(T), Point }
  deriving (Eq, Show)

-- | A single variant inside an enum declaration
data Variant = Variant Name [Type]  -- ^ Circle(Int, Int) or Point (empty list)
  deriving (Eq, Show)

-- | Code block
newtype Block = Block [LStmt]
  deriving (Eq, Show)

-- | Function parameters
data Param = Param Name (Maybe Type)
  deriving (Eq, Show)

-- | Function arguments
data Arg = Arg LExpr
  deriving (Eq, Show)

-- | Language expression
data Expr
  = EVar Name
  | ELit Literal
  | ELam [Param] (Maybe Type) LExpr
  | ECall LExpr [Arg]
  | EMember LExpr Name
  | EIndex LExpr LExpr
  | EObject [(Name, LExpr)]
  | EArray [LExpr]
  | EAssign LExpr LExpr
  | EUnary UnOp LExpr
  | EBinary BinOp LExpr LExpr
  | EIfExpr LExpr LExpr LExpr
  | EParens LExpr
  | EVariant Name Name [LExpr]  -- ^ e.g. Shape::Circle(5, 10)
  | EMatch LExpr [MatchArm]    -- ^ e.g. match (expr) { arms }
  deriving (Eq, Show)

-- | A single arm in a match expression
data MatchArm = MatchArm Pattern LExpr
  deriving (Eq, Show)

-- | Pattern in a match arm (flat, non-nested)
data Pattern
  = PVariant Name Name [Name]  -- ^ e.g. EnumName::VariantName(x, y, z)
  | PWild                      -- ^ e.g. _ (catch-all)
  deriving (Eq, Show)

-- | Literals
data Literal
  = LInt Integer
  | LBool Bool
  | LString Text
  | LNull
  deriving (Eq, Show)

-- | Unary operations
data UnOp = Neg | Not
  deriving (Eq, Show)

-- | Binary operations
data BinOp
  = Mul | Div | Mod
  | Add | Sub
  | Lt | Lte | Gt | Gte
  | Eq | Neq
  | And | Or
  deriving (Eq, Show)

-- | Language types
data Type
  = TInt
  | TBool
  | TString
  | TVar Name
  | TArray Type
  | TObject [(Name, Type)]
  | TApp Name [Type]       -- e.g. Result<Int, String>
  | TFun [Type] Type       -- e.g. (a, b) -> c
  deriving (Eq, Show)
