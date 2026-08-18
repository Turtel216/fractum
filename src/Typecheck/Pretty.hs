{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Typecheck.Pretty
-- Description : User-facing rendering of internal types.
--
-- 'Typecheck.Types.IType' has a 'Show' instance, but it is a debugging
-- representation. These functions produce the surface-like spelling that
-- belongs in a diagnostic: @(Int, Int) -> Int@ rather than @TFunT [..] ..@.
module Typecheck.Pretty
  ( prettyType,
    prettyTVar,
  )
where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Diagnostic.Types (tshow)
import Typecheck.Types (IType (..), TVarId)

-- | Render an 'IType' as a user-friendly string.
prettyType :: IType -> Text
prettyType = \case
  TV n -> prettyTVar n
  TIntT -> "Int"
  TFloatT -> "Float"
  TBoolT -> "Bool"
  TStringT -> "String"
  TNullT -> "Null"
  TFunT [] r -> "() -> " <> prettyType r
  TFunT [a] r -> prettyTypeAtom a <> " -> " <> prettyType r
  TFunT as r -> "(" <> T.intercalate ", " (map prettyType as) <> ") -> " <> prettyType r
  TArrayT t -> "[" <> prettyType t <> "]"
  TObjectT fs ->
    "{ " <> T.intercalate ", " [k <> ": " <> prettyType v | (k, v) <- M.toList fs] <> " }"
  TCon n [] -> n
  TCon n ts -> n <> "<" <> T.intercalate ", " (map prettyType ts) <> ">"

-- | Parenthesise function types when they appear as arguments to another
-- function type.
prettyTypeAtom :: IType -> Text
prettyTypeAtom t@(TFunT {}) = "(" <> prettyType t <> ")"
prettyTypeAtom t = prettyType t

-- | Render a type variable id as a lowercase letter (a, b, …, z, a1, b1, …).
prettyTVar :: TVarId -> Text
prettyTVar n
  | n < 26 = T.singleton (toEnum (fromEnum 'a' + n))
  | otherwise =
      T.singleton (toEnum (fromEnum 'a' + (n `mod` 26)))
        <> tshow (n `div` 26)
