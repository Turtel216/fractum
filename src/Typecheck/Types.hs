{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Typecheck.Types
-- Description : The internal type representation, type schemes, and substitutions.
--
-- 'IType' is the checker's internal representation of a type (as opposed to
-- 'Ast.Type', the surface syntax written by the user). A 'Scheme' closes over
-- the type variables generalized by let-polymorphism. 'Subst' is a finite
-- map from type variables to the types they have been unified with; the
-- 'Types' class describes how to query and apply one over a structure.
module Typecheck.Types
  ( TVarId,
    IType (..),
    Scheme (..),
    TypeEnv,
    Subst (..),
    emptySubst,
    compose,
    Types (..),
  )
where

import Ast (Mutability)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

type TVarId = Int

data IType
  = TV TVarId
  | TIntT
  | TFloatT
  | TBoolT
  | TStringT
  | TNullT
  | TFunT [IType] IType
  | TArrayT IType
  | TObjectT (M.Map Text IType)
  | TCon Text [IType]
  deriving (Eq, Ord)

instance Show IType where
  show = \case
    TV n -> "t" <> show n
    TIntT -> "Int"
    TFloatT -> "Float"
    TBoolT -> "Bool"
    TStringT -> "String"
    TNullT -> "Null"
    TFunT as r -> "(" <> unwords (map show as) <> ") -> " <> show r
    TArrayT t -> "[" <> show t <> "]"
    TObjectT fs -> "{ " <> unwords [T.unpack k <> ": " <> show v | (k, v) <- M.toList fs] <> " }"
    TCon n ts -> T.unpack n <> "<" <> unwords (map show ts) <> ">"

-- | A polymorphic type, closed over the type variables generalized at a
-- let-binding: @forall as. t@.
data Scheme = Forall [TVarId] IType
  deriving (Eq, Show)

-- | Each binding stores its type scheme and whether it is mutable.
type TypeEnv = M.Map Text (Scheme, Mutability)

-- | A finite map from type variables to the types unified with them.
newtype Subst = Subst (M.Map TVarId IType)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst M.empty

-- | @compose newer older@: apply @newer@ throughout @older@'s range, then
-- prefer @newer@'s own bindings on key clashes.
compose :: Subst -> Subst -> Subst
compose s1@(Subst a) (Subst b) =
  Subst (M.map (apply s1) b <> a)

-- | Structures that mention type variables and can have a substitution
-- applied throughout them.
class Types a where
  ftv :: a -> S.Set TVarId
  apply :: Subst -> a -> a

instance Types IType where
  ftv = \case
    TV n -> S.singleton n
    TIntT -> mempty
    TFloatT -> mempty
    TBoolT -> mempty
    TStringT -> mempty
    TNullT -> mempty
    TFunT as r -> S.unions (map ftv (r : as))
    TArrayT t -> ftv t
    TObjectT fs -> S.unions (map ftv (M.elems fs))
    TCon _ ts -> S.unions (map ftv ts)

  apply (Subst s) t = case t of
    TV n -> M.findWithDefault t n s
    TIntT -> TIntT
    TFloatT -> TFloatT
    TBoolT -> TBoolT
    TStringT -> TStringT
    TNullT -> TNullT
    TFunT as r -> TFunT (map go as) (go r)
    TArrayT x -> TArrayT (go x)
    TObjectT fs -> TObjectT (M.map go fs)
    TCon n ts -> TCon n (map go ts)
    where
      go = apply (Subst s)

instance Types Scheme where
  ftv (Forall as t) = ftv t `S.difference` S.fromList as
  apply (Subst s) (Forall as t) =
    let s' = Subst (foldr M.delete s as)
     in Forall as (apply s' t)

instance Types (Scheme, Mutability) where
  ftv (sch, _) = ftv sch
  apply s (sch, m) = (apply s sch, m)

instance Types TypeEnv where
  ftv env = S.unions (map ftv (M.elems env))
  apply s = M.map (apply s)
