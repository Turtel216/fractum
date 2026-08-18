{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Typecheck.Error
-- Description : Rich, span-aware type errors produced by the checker.
--
-- The 'Diagnostic.Types.ToDiagnostic' instance here is the checker's half of
-- the contract with "Diagnostic": it decides the wording, the code and the
-- notes, and the renderer decides only the layout. "Parser.Error" holds the
-- syntactic half of the same contract, which is why both phases produce
-- diagnostics that look identical to the user.
module Typecheck.Error
  ( TypeError (..),
    TypeErrorKind (..),
    Note (..),
  )
where

import Ast (Span)
import Data.Text (Text)
import qualified Data.Text as T
import Diagnostic.Types (Diagnostic (..), Note (..), ToDiagnostic (..), tshow)
import Typecheck.Pretty (prettyTVar, prettyType)
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
  | -- | (source type, target type) of an unsupported `as` cast
    InvalidCast IType IType
  | OtherError Text
  deriving (Eq, Show)

instance ToDiagnostic TypeError where
  toDiagnostic (TypeError mSpan kind notes) =
    Diagnostic
      { diagCode = errorCode kind,
        diagTitle = errorTitle kind,
        diagSpan = mSpan,
        diagDetail = errorDetail kind,
        diagNotes = notes
      }

-- | Stable error code for each error kind.
errorCode :: TypeErrorKind -> Text
errorCode = \case
  TypeMismatch {} -> "E0001"
  InfiniteType {} -> "E0002"
  UnboundVariable {} -> "E0003"
  MissingField {} -> "E0004"
  DuplicateBinding {} -> "E0005"
  ImmutableAssign {} -> "E0006"
  UnboundType {} -> "E0007"
  DuplicateType {} -> "E0008"
  TypeArityMismatch {} -> "E0009"
  NonExhaustiveMatch {} -> "E0010"
  UnknownVariant {} -> "E0011"
  UnknownEnum {} -> "E0012"
  VariantArityMismatch {} -> "E0013"
  ModuleNotFound {} -> "E0014"
  UnboundImport {} -> "E0015"
  CircularImport {} -> "E0016"
  InvalidCast {} -> "E0017"
  OtherError {} -> "E0099"

-- | Short human-readable title for the error kind.
errorTitle :: TypeErrorKind -> Text
errorTitle = \case
  TypeMismatch {} -> "type mismatch"
  InfiniteType {} -> "infinite type"
  UnboundVariable {} -> "cannot find value in this scope"
  MissingField {} -> "missing field"
  DuplicateBinding {} -> "duplicate definition"
  ImmutableAssign {} -> "cannot assign to immutable variable"
  UnboundType {} -> "undefined type"
  DuplicateType {} -> "duplicate type definition"
  TypeArityMismatch {} -> "wrong number of type arguments"
  NonExhaustiveMatch {} -> "non-exhaustive match"
  UnknownVariant {} -> "unknown variant"
  UnknownEnum {} -> "unknown enum"
  VariantArityMismatch {} -> "wrong number of variant fields"
  ModuleNotFound {} -> "module not found"
  UnboundImport {} -> "no exported member"
  CircularImport {} -> "circular import"
  InvalidCast {} -> "invalid cast"
  OtherError {} -> "type error"

-- | Detailed message shown under the source underline.
errorDetail :: TypeErrorKind -> Text
errorDetail = \case
  TypeMismatch t1 t2 ->
    "expected `" <> prettyType t2 <> "`, found `" <> prettyType t1 <> "`"
  InfiniteType v t ->
    "type variable `" <> prettyTVar v <> "` occurs in `" <> prettyType t <> "`"
  UnboundVariable _name ->
    "not found in this scope"
  MissingField name ->
    "no field `" <> name <> "` on this type"
  DuplicateBinding name ->
    "`" <> name <> "` is already defined in this scope"
  ImmutableAssign name ->
    "cannot assign to `" <> name <> "`"
  UnboundType name ->
    "type `" <> name <> "` is not defined"
  DuplicateType name ->
    "type `" <> name <> "` is already defined"
  TypeArityMismatch name expected got ->
    "`"
      <> name
      <> "` expects "
      <> tshow expected
      <> " type argument(s) but "
      <> tshow got
      <> " were given"
  NonExhaustiveMatch name missing ->
    "match on `" <> name <> "` is not exhaustive, missing: "
      <> T.intercalate ", " ["`" <> v <> "`" | v <- missing]
  UnknownVariant enumN varN ->
    "variant `" <> varN <> "` does not exist on enum `" <> enumN <> "`"
  UnknownEnum name ->
    "enum `" <> name <> "` is not defined"
  VariantArityMismatch enumN varN expected got ->
    "variant `"
      <> varN
      <> "` of enum `"
      <> enumN
      <> "` expects "
      <> tshow expected
      <> " field(s) but "
      <> tshow got
      <> " were given"
  ModuleNotFound path ->
    "cannot find module `" <> path <> "`"
  UnboundImport name path ->
    "module `" <> path <> "` has no exported member `" <> name <> "`"
  CircularImport cyclePath ->
    "modules form a cycle: " <> T.intercalate " -> " cyclePath
  InvalidCast from to ->
    "cannot cast `"
      <> prettyType from
      <> "` to `"
      <> prettyType to
      <> "` -- only conversions between `Int` and `Float` are supported"
  OtherError msg -> msg
