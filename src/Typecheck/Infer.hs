{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Typecheck.Infer
-- Description : The inference monad: fresh variables, unification, and generalization.
--
-- 'Infer' is a stack of @ExceptT TypeError@ over @State InferState@. Unlike a
-- textbook substitution-passing presentation of Algorithm W, the accumulated
-- 'Subst' lives directly in 'InferState' rather than being threaded and
-- @compose@d through every function's return value: 'unify' folds each new
-- unifier into the state, and 'zonk' reads it back out. Because inference
-- here is a single strict left-to-right pass with no backtracking, the
-- substitution in scope at any point is always exactly what explicit
-- threading would have produced, so this is a pure simplification with no
-- change in behaviour.
module Typecheck.Infer
  ( Infer,
    InferState (..),
    TypeDeclEnv,
    EnumDeclEnv,
    VariantSig (..),
    runInfer,
    fresh,
    withCurrentSpan,
    throwSpanned,
    zonk,
    instantiate,
    generalize,
    unify,
  )
where

import Ast (Name, Span, Type)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Typecheck.Error
import Typecheck.Types

-- | Registered type aliases: name -> (type parameters, body).
type TypeDeclEnv = M.Map Name ([Name], Type)

-- | Signature of a single enum variant (stored with surface types for instantiation).
data VariantSig = VariantSig Name [Type]
  deriving (Eq, Show)

-- | Registered enum declarations: name -> (type params, variant signatures).
type EnumDeclEnv = M.Map Name ([Name], [VariantSig])

data InferState = InferState
  { count :: !Int,
    typeDecls :: !TypeDeclEnv,
    enumDecls :: !EnumDeclEnv,
    -- | span of the AST node currently being analysed
    currentSpan :: !(Maybe Span),
    -- | explicit type variable bindings (from generic functions)
    tyVarBinds :: !(M.Map Name IType),
    -- | substitution accumulated from every 'unify' call so far
    subst :: !Subst
  }

newtype Infer a = Infer {unInfer :: ExceptT TypeError (State InferState) a}
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState)

runInfer :: Infer a -> Either TypeError a
runInfer m = evalState (runExceptT (unInfer m)) (InferState 0 M.empty M.empty Nothing M.empty emptySubst)

fresh :: Infer IType
fresh = do
  st <- get
  put st {count = count st + 1}
  pure (TV (count st))

-- | Execute @m@ with 'currentSpan' set to @sp@, restoring the old span on
-- return.  If @m@ throws, the span is /not/ restored — which is fine under
-- the fail-fast error model.
withCurrentSpan :: Span -> Infer a -> Infer a
withCurrentSpan sp m = do
  old <- gets currentSpan
  modify' (\s -> s {currentSpan = Just sp})
  result <- m
  modify' (\s -> s {currentSpan = old})
  pure result

-- | Throw a 'TypeError' using the current span stored in 'InferState'.
throwSpanned :: TypeErrorKind -> [Note] -> Infer a
throwSpanned kind notes = do
  sp <- gets currentSpan
  throwError (TypeError sp kind notes)

-- | Apply the substitution accumulated so far to a structure. Because 'unify'
-- keeps 'subst' up to date as inference proceeds, this always reflects every
-- unification performed up to this point in the pass.
zonk :: (Types a) => a -> Infer a
zonk x = do
  s <- gets subst
  pure (apply s x)

-- | Instantiate a scheme with fresh type variables for its generalized
-- variables. Any variable free in the scheme's body but *not* generalized is
-- resolved against the current substitution, so monomorphic bindings whose
-- type has since been refined by unification still come back up to date.
--
-- Zonking goes through the scheme itself (via its 'Types' instance) rather
-- than the raw body, since that instance deletes the scheme's own bound
-- variables from the substitution first. That protection matters here: type
-- variable ids are drawn from one global counter, so an unrelated binding's
-- generalized variable can numerically collide with this scheme's; zonking
-- the raw body would let that unrelated substitution hijack a bound
-- variable it has nothing to do with.
instantiate :: Scheme -> Infer IType
instantiate sch = do
  Forall vars t' <- zonk sch
  reps <- mapM (const fresh) vars
  let s = Subst (M.fromList (zip vars reps))
  pure (apply s t')

-- | Generalize a type over every variable free in it but not free in the
-- (zonked) environment, producing a let-polymorphic 'Scheme'.
generalize :: TypeEnv -> IType -> Infer Scheme
generalize env t = do
  envZonked <- zonk env
  tZonked <- zonk t
  let vars = S.toList (ftv tZonked `S.difference` ftv envZonked)
  pure (Forall vars tZonked)

-- | Unify two types against the substitution accumulated so far, folding the
-- resulting unifier back into the state. Errors report the types as they
-- were at the moment of the mismatch (i.e. already zonked).
unify :: IType -> IType -> Infer ()
unify t1 t2 = do
  s <- gets subst
  u <- unifyStructural (apply s t1) (apply s t2)
  modify' (\st -> st {subst = compose u (subst st)})

unifyStructural :: IType -> IType -> Infer Subst
unifyStructural t1 t2 = case (t1, t2) of
  (TFunT as1 r1, TFunT as2 r2)
    | length as1 == length as2 -> unifyManyStructural (as1 <> [r1]) (as2 <> [r2])
    | otherwise -> throwSpanned (TypeMismatch t1 t2) []
  (TV v, t) -> bind v t
  (t, TV v) -> bind v t
  (TIntT, TIntT) -> pure emptySubst
  (TFloatT, TFloatT) -> pure emptySubst
  (TBoolT, TBoolT) -> pure emptySubst
  (TStringT, TStringT) -> pure emptySubst
  (TNullT, TNullT) -> pure emptySubst
  (TArrayT a, TArrayT b) -> unifyStructural a b
  (TObjectT fa, TObjectT fb)
    | M.keysSet fa == M.keysSet fb ->
        unifyManyStructural (M.elems fa) (M.elems fb)
    | otherwise -> throwSpanned (TypeMismatch t1 t2) []
  (TCon n1 as1, TCon n2 as2)
    | n1 == n2 && length as1 == length as2 -> unifyManyStructural as1 as2
    | otherwise -> throwSpanned (TypeMismatch t1 t2) []
  _ -> throwSpanned (TypeMismatch t1 t2) []

unifyManyStructural :: [IType] -> [IType] -> Infer Subst
unifyManyStructural [] [] = pure emptySubst
unifyManyStructural (t1 : ts1) (t2 : ts2) = do
  s1 <- unifyStructural t1 t2
  s2 <- unifyManyStructural (map (apply s1) ts1) (map (apply s1) ts2)
  pure (compose s2 s1)
unifyManyStructural _ _ = throwSpanned (OtherError "arity mismatch in unifyMany") []

bind :: TVarId -> IType -> Infer Subst
bind a t
  | t == TV a = pure emptySubst
  | a `S.member` ftv t = throwSpanned (InfiniteType a t) []
  | otherwise = pure (Subst (M.singleton a t))
