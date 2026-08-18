{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Typecheck.Surface
-- Description : Conversion between surface 'Ast.Type' and internal 'IType', and enum lookups.
module Typecheck.Surface
  ( substSurfaceType,
    fromSurfaceType,
    toSurfaceType,
    resolveEnumType,
    resolveEnumByName,
  )
where

import Ast
import Control.Monad.State (gets)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Typecheck.Error
import Typecheck.Infer
import Typecheck.Types

-- | Substitute type variables in a surface Type (used for parametric alias expansion).
-- Type parameters may appear as either @TVar n@ (lowercase) or @TApp n []@ (uppercase
-- with no args), so both forms are checked against the substitution map.
substSurfaceType :: M.Map Name Type -> Type -> Type
substSurfaceType m = go
  where
    go = \case
      TVar n -> M.findWithDefault (TVar n) n m
      TApp n [] | Just t <- M.lookup n m -> t -- type parameter (uppercase)
      TArray t -> TArray (go t)
      TObject fs -> TObject [(k, go v) | (k, v) <- fs]
      TApp n as -> TApp n (map go as)
      TFun as r -> TFun (map go as) (go r)
      t -> t -- TInt, TBool, TString pass through

fromSurfaceType :: Type -> Infer IType
fromSurfaceType = \case
  TInt -> pure TIntT
  TFloat -> pure TFloatT
  TBool -> pure TBoolT
  TString -> pure TStringT
  TVar _ -> fresh -- surface named vars treated as fresh unknowns
  TArray t -> TArrayT <$> fromSurfaceType t
  TObject fields -> TObjectT . M.fromList <$> mapM go fields
    where
      go (k, v) = (k,) <$> fromSurfaceType v
  TApp n args -> do
    -- Check explicit type variable bindings first (from generic function decls)
    binds <- gets tyVarBinds
    case M.lookup n binds of
      Just itype | null args -> pure itype -- return pre-allocated TV
      _ -> do
        decls <- gets typeDecls
        enums <- gets enumDecls
        case M.lookup n decls of
          Just (params, body)
            | length params /= length args ->
                throwSpanned (TypeArityMismatch n (length params) (length args)) []
            | otherwise -> do
                let substMap = M.fromList (zip params args)
                    expanded = substSurfaceType substMap body
                fromSurfaceType expanded
          Nothing -> case M.lookup n enums of
            Just (params, _)
              | length params /= length args ->
                  throwSpanned (TypeArityMismatch n (length params) (length args)) []
              | otherwise ->
                  TCon n <$> mapM fromSurfaceType args
            Nothing -> throwSpanned (UnboundType n) []
  TFun as r -> TFunT <$> mapM fromSurfaceType as <*> fromSurfaceType r

-- | Convert an internal type back to a surface Type for use in
-- substitution maps during enum instantiation.
toSurfaceType :: IType -> Type
toSurfaceType = \case
  TV n -> TVar (T.pack ("_tv" <> show n))
  TIntT -> TInt
  TFloatT -> TFloat
  TBoolT -> TBool
  TStringT -> TString
  TNullT -> TVar "_null" -- no surface Null type; use a placeholder
  TFunT as r -> TFun (map toSurfaceType as) (toSurfaceType r)
  TArrayT t -> TArray (toSurfaceType t)
  TObjectT fs -> TObject [(k, toSurfaceType v) | (k, v) <- M.toList fs]
  TCon n ts -> TApp n (map toSurfaceType ts)

-- | Resolve the scrutinee type of a match expression to its enum declaration.
-- Returns (enumName, typeParams, variantSigs, concreteTypeArgs).
resolveEnumType :: IType -> Infer (Name, [Name], [VariantSig], [IType])
resolveEnumType (TCon name tyArgs) = do
  enums <- gets enumDecls
  case M.lookup name enums of
    Nothing -> throwSpanned (UnknownEnum name) []
    Just (tyParams, variants) -> pure (name, tyParams, variants, tyArgs)
resolveEnumType t =
  throwSpanned (OtherError ("expected enum type in match, found `" <> T.pack (show t) <> "`")) []

-- | Resolve an enum referenced by its literal source name (as written by the
-- user in @EnumName::Variant@ or a pattern), following at most one level of
-- type-alias indirection. This lets an imported enum, re-exposed locally as a
-- trivial forwarding @type Local = Target<...>@ alias, still be constructed
-- and matched on under its local name.
resolveEnumByName :: Name -> Infer (Name, [Name], [VariantSig])
resolveEnumByName name = do
  enums <- gets enumDecls
  case M.lookup name enums of
    Just (tyParams, variants) -> pure (name, tyParams, variants)
    Nothing -> do
      decls <- gets typeDecls
      case M.lookup name decls of
        Just (_, TApp target _) -> do
          enums' <- gets enumDecls
          case M.lookup target enums' of
            Just (tyParams, variants) -> pure (target, tyParams, variants)
            Nothing -> throwSpanned (UnknownEnum name) []
        _ -> throwSpanned (UnknownEnum name) []
