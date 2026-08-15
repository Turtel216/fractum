-- |
-- Module      : Typecheck.Error
-- Description : Rich, span-aware type errors produced by the checker.
module Typecheck.Error
  ( TypeError (..),
    TypeErrorKind (..),
    Note (..),
  )
where

import Ast (Span)
import Data.Text (Text)
import Typecheck.Types (IType, TVarId)

-- | A rich type error with source location and contextual notes.
data TypeError = TypeError
  { -- | primary error location
    teSpan :: Maybe Span,
    -- | what went wrong
    teKind :: TypeErrorKind,
    -- | secondary explanations
    teNotes :: [Note]
  }
  deriving (Eq, Show)

-- | Classification of type errors.
data TypeErrorKind
  = -- | (found, expected)
    TypeMismatch IType IType
  | InfiniteType TVarId IType
  | UnboundVariable Text
  | MissingField Text
  | DuplicateBinding Text
  | ImmutableAssign Text
  | UnboundType Text
  | DuplicateType Text
  | -- | name, expected arity, got
    TypeArityMismatch Text Int Int
  | -- | enum name, missing variants
    NonExhaustiveMatch Text [Text]
  | -- | enum name, variant name
    UnknownVariant Text Text
  | -- | enum name
    UnknownEnum Text
  | -- | enum, variant, expected, got
    VariantArityMismatch Text Text Int Int
  | -- | unresolved relative import path
    ModuleNotFound Text
  | -- | imported name, module path
    UnboundImport Text Text
  | -- | cycle of module paths
    CircularImport [Text]
  | OtherError Text
  deriving (Eq, Show)

-- | Additional notes attached to a type error.
data Note
  = -- | plain @note:@ line
    NoteText Text
  | -- | @help:@ suggestion
    NoteHelp Text
  | -- | secondary source location with message
    NoteSpan Span Text
  deriving (Eq, Show)
