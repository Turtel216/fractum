{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Typecheck
Description : Semantic analysis, environment tracking, and type inference engine.
Stability   : experimental

This module implements a robust, pass-based typechecker based on an extended 
Hindley-Milner (Algorithm W) type inference algorithm. It transforms an untyped
surface AST ('Ast') into heavily validated semantic constructs, producing a final
'TypeEnv' or short-circuiting with rich, span-aware errors.

== Architecture Overview

* __Inference Monad ('Infer'):__ 
    Typehecking operates within a custom 'Infer' monad, a stack of @ExceptT TypeError@ 
    over @State InferState@. This tracks the supply of fresh type variables ('TVarId'), 
    a registry of user-defined type aliases ('TypeDeclEnv'), and the current AST 
    node span to ensure localized error reporting.

* __Type System ('IType'):__
    Supports primitives (Int, Bool, String, Null), first-class functions ('TFunT'), 
    arrays ('TArrayT'), and structural record/object types ('TObjectT'). Parametric 
    polymorphism is supported via standard let-generalization ('Scheme').

* __Unification & Substitution:__
    Substitution ('Subst') is built compositionally. Unification uses standard 
    structural equality with an occurs check to prevent infinite types. Note that 
    object unification currently requires strict field equality (exact structural match)
    rather than subtyping or row polymorphism.

* __Mutability Tracking:__
    The 'TypeEnv' tracks not just type schems, but also 'Mutability'. The inference 
    rules for 'EAssign' explicitly guard against reassigning immutable bindings, 
    emitting contextual 'NoteHelp' suggestions when violated.

* __Error Handling ('TypeError'):__
    Errors are designed for modern compiler diagnostics (e.g., in the style of Rust or Elm). 
    The fail-fast 'ExceptT' model bails on the first unification or binding error, 
    attaching primary spans ('teSpan'), the failure kind ('teKind'), and secondary 
    contextual notes ('Note').

== Key Entry Points

* 'inferProgram': Main pipeline entry. Processes a sequence of top-level statements, 
    populating declarations and producing a final exported 'TypeEnv'.
* 'inferExprInEmpty': Utility for testing. Infers the type of an 
    isolated expression against a base prelude environment.
-}
module Typecheck
  ( TypeError (..),
    TypeErrorKind (..),
    Note (..),
    IType (..),
    TVarId,
    Scheme (..),
    inferProgram,
    inferExprInEmpty,
  )
where

import Ast
import Control.Monad (foldM, when, unless)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

type TVarId = Int

data IType
  = TV TVarId
  | TIntT
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
    TBoolT -> "Bool"
    TStringT -> "String"
    TNullT -> "Null"
    TFunT as r -> "(" <> unwords (map show as) <> ") -> " <> show r
    TArrayT t -> "[" <> show t <> "]"
    TObjectT fs -> "{ " <> unwords [T.unpack k <> ": " <> show v | (k, v) <- M.toList fs] <> " }"
    TCon n ts -> T.unpack n <> "<" <> unwords (map show ts) <> ">"

data Scheme = Forall [TVarId] IType
  deriving (Eq, Show)

-- | Each binding stores its type scheme and whether it is mutable.
type TypeEnv = M.Map Text (Scheme, Mutability)

newtype Subst = Subst (M.Map TVarId IType)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst M.empty

compose :: Subst -> Subst -> Subst
compose s1@(Subst a) (Subst b) =
  Subst (M.map (apply s1) b <> a)

class Types a where
  ftv :: a -> S.Set TVarId
  apply :: Subst -> a -> a

instance Types IType where
  ftv = \case
    TV n -> S.singleton n
    TIntT -> mempty
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

-- | A rich type error with source location and contextual notes.
data TypeError = TypeError
  { teSpan  :: Maybe Span     -- ^ primary error location
  , teKind  :: TypeErrorKind  -- ^ what went wrong
  , teNotes :: [Note]         -- ^ secondary explanations
  }
  deriving (Eq, Show)

-- | Classification of type errors.
data TypeErrorKind
  = TypeMismatch IType IType          -- ^ (found, expected)
  | InfiniteType TVarId IType
  | UnboundVariable Text
  | MissingField Text
  | DuplicateBinding Text
  | ImmutableAssign Text
  | UnboundType Text
  | DuplicateType Text
  | TypeArityMismatch Text Int Int    -- ^ name, expected arity, got
  | NonExhaustiveMatch Text [Text]    -- ^ enum name, missing variants
  | UnknownVariant Text Text          -- ^ enum name, variant name
  | UnknownEnum Text                  -- ^ enum name
  | VariantArityMismatch Text Text Int Int  -- ^ enum, variant, expected, got
  | ModuleNotFound Text                     -- ^ unresolved relative import path
  | UnboundImport Text Text                 -- ^ imported name, module path
  | CircularImport [Text]                   -- ^ cycle of module paths
  | OtherError Text
  deriving (Eq, Show)

-- | Additional notes attached to a type error.
data Note
  = NoteText Text                     -- ^ plain @note:@ line
  | NoteHelp Text                     -- ^ @help:@ suggestion
  | NoteSpan Span Text                -- ^ secondary source location with message
  deriving (Eq, Show)

-- | Registered type aliases: name → (type parameters, body)
type TypeDeclEnv = M.Map Text ([Name], Type)

-- | Signature of a single enum variant (stored with surface types for instantiation).
data VariantSig = VariantSig Name [Type]
  deriving (Eq, Show)

-- | Registered enum declarations: name → (type params, variant signatures).
type EnumDeclEnv = M.Map Text ([Name], [VariantSig])

data InferState = InferState
  { count       :: Int
  , typeDecls   :: TypeDeclEnv
  , enumDecls   :: EnumDeclEnv
  , currentSpan :: Maybe Span    -- ^ span of the AST node currently being analysed
  , tyVarBinds  :: M.Map Name IType  -- ^ explicit type variable bindings (from generic functions)
  }

newtype Infer a = Infer {unInfer :: ExceptT TypeError (State InferState) a}
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState)

runInfer :: Infer a -> Either TypeError a
runInfer m = evalState (runExceptT (unInfer m)) (InferState 0 M.empty M.empty Nothing M.empty)

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
  modify' (\s -> s { currentSpan = Just sp })
  result <- m
  modify' (\s -> s { currentSpan = old })
  pure result

-- | Throw a 'TypeError' using the current span stored in 'InferState'.
throwSpanned :: TypeErrorKind -> [Note] -> Infer a
throwSpanned kind notes = do
  sp <- gets currentSpan
  throwError (TypeError sp kind notes)

instantiate :: Scheme -> Infer IType
instantiate (Forall vars t) = do
  reps <- mapM (const fresh) vars
  let s = Subst (M.fromList (zip vars reps))
  pure (apply s t)

generalize :: TypeEnv -> IType -> Scheme
generalize env t =
  let envFtv = S.unions [ftv sch | (sch, _) <- M.elems env]
      vars = S.toList (ftv t `S.difference` envFtv)
   in Forall vars t

unify :: IType -> IType -> Infer Subst
unify t1 t2 = case (t1, t2) of
  (TFunT as1 r1, TFunT as2 r2)
    | length as1 == length as2 -> unifyMany (as1 <> [r1]) (as2 <> [r2])
    | otherwise -> throwSpanned (TypeMismatch t1 t2) []
  (TV v, t) -> bind v t
  (t, TV v) -> bind v t
  (TIntT, TIntT) -> pure emptySubst
  (TBoolT, TBoolT) -> pure emptySubst
  (TStringT, TStringT) -> pure emptySubst
  (TNullT, TNullT) -> pure emptySubst
  (TArrayT a, TArrayT b) -> unify a b
  (TObjectT fa, TObjectT fb)
    | M.keysSet fa == M.keysSet fb ->
        unifyMany (M.elems fa) (M.elems fb)
    | otherwise -> throwSpanned (TypeMismatch t1 t2) []
  (TCon n1 as1, TCon n2 as2)
    | n1 == n2 && length as1 == length as2 -> unifyMany as1 as2
    | otherwise -> throwSpanned (TypeMismatch t1 t2) []
  _ -> throwSpanned (TypeMismatch t1 t2) []

unifyMany :: [IType] -> [IType] -> Infer Subst
unifyMany [] [] = pure emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (map (apply s1) ts1) (map (apply s1) ts2)
  pure (compose s2 s1)
unifyMany _ _ = throwSpanned (OtherError "arity mismatch in unifyMany") []

bind :: TVarId -> IType -> Infer Subst
bind a t
  | t == TV a = pure emptySubst
  | a `S.member` ftv t = throwSpanned (InfiniteType a t) []
  | otherwise = pure (Subst (M.singleton a t))

-- | Substitute type variables in a surface Type (used for parametric alias expansion).
-- Type parameters may appear as either @TVar n@ (lowercase) or @TApp n []@ (uppercase
-- with no args), so both forms are checked against the substitution map.
substSurfaceType :: M.Map Name Type -> Type -> Type
substSurfaceType m = go
  where
    go = \case
      TVar n      -> M.findWithDefault (TVar n) n m
      TApp n [] | Just t <- M.lookup n m -> t   -- type parameter (uppercase)
      TArray t    -> TArray (go t)
      TObject fs  -> TObject [(k, go v) | (k, v) <- fs]
      TApp n as   -> TApp n (map go as)
      TFun as r   -> TFun (map go as) (go r)
      t           -> t  -- TInt, TBool, TString pass through

fromSurfaceType :: Type -> Infer IType
fromSurfaceType = \case
  TInt -> pure TIntT
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
      Just itype | null args -> pure itype  -- return pre-allocated TV
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
  TV n       -> TVar (T.pack ("_tv" <> show n))
  TIntT      -> TInt
  TBoolT     -> TBool
  TStringT   -> TString
  TNullT     -> TVar "_null"   -- no surface Null type; use a placeholder
  TFunT as r -> TFun (map toSurfaceType as) (toSurfaceType r)
  TArrayT t  -> TArray (toSurfaceType t)
  TObjectT fs -> TObject [(k, toSurfaceType v) | (k, v) <- M.toList fs]
  TCon n ts  -> TApp n (map toSurfaceType ts)

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

inferExpr :: TypeEnv -> LExpr -> Infer (Subst, IType)
inferExpr env (Located sp expr) = withCurrentSpan sp $ case expr of
  EVar x ->
    case M.lookup x env of
      Nothing -> throwError (TypeError (Just sp) (UnboundVariable x) [])
      Just (sch, _) -> do t <- instantiate sch; pure (emptySubst, t)

  ELit l -> case l of
    LInt _ -> pure (emptySubst, TIntT)
    LBool _ -> pure (emptySubst, TBoolT)
    LString _ -> pure (emptySubst, TStringT)
    LNull -> pure (emptySubst, TNullT)

  EParens e -> inferExpr env e

  EArray es -> do
    tv <- fresh
    (s, ts) <- inferList env es
    s' <-
      foldM
        ( \acc t -> do
            u <- unify (apply acc tv) (apply acc t)
            pure (compose u acc)
        )
        s
        ts
    pure (s', apply s' (TArrayT tv))

  EObject fs -> do
    (s, typed) <- foldM step (emptySubst, []) fs
    pure (s, TObjectT (M.fromList typed))
    where
      step (sAcc, out) (k, e) = do
        (sE, tE) <- inferExpr (apply sAcc env) e
        let sAll = compose sE sAcc
        pure (sAll, out <> [(k, apply sAll tE)])

  ELam params mRet body -> do
    -- build parameter types
    (env', ptys) <- foldM addParam (env, []) params
    (sBody, tBody) <- inferExpr env' body

    -- optional return type annotation
    sRet <- case mRet of
      Nothing -> pure emptySubst
      Just rt -> do
        rt' <- fromSurfaceType rt
        unify (apply sBody tBody) rt'

    let sAll = compose sRet sBody
    pure (sAll, TFunT (map (apply sAll) ptys) (apply sAll tBody))
    where
      addParam (e, ts) (Param n mt) = do
        t <- maybe fresh fromSurfaceType mt
        pure (M.insert n (Forall [] t, Immutable) e, ts <> [t])

  ECall fn args -> do
    (sFn, tFn) <- inferExpr env fn
    (sArgs, argTs) <- inferArgList (apply sFn env) args
    ret <- fresh
    sCall <- unify (TFunT argTs ret) (apply sArgs tFn)
    let sAll = compose sCall (compose sArgs sFn)
    pure (sAll, apply sAll ret)

  EMember obj field -> do
    (sObj, tObj0) <- inferExpr env obj
    let tObj = apply sObj tObj0
    case tObj of
      TObjectT fs ->
        case M.lookup field fs of
          Just tf -> pure (sObj, tf)
          Nothing -> throwError (TypeError (Just sp) (MissingField field) [])
      -- fallback for unknown/non-concrete object terms
      _ -> do
        tv <- fresh
        sField <- unify tObj (TObjectT (M.singleton field tv))
        let sAll = compose sField sObj
        pure (sAll, apply sAll tv)

  EIndex arr ix -> do
    (s1, tArr) <- inferExpr env arr
    (s2, tIx) <- inferExpr (apply s1 env) ix
    sIx <- unify tIx TIntT
    tv <- fresh
    sArr <- unify (apply sIx tArr) (TArrayT tv)
    let sAll = compose sArr (compose sIx (compose s2 s1))
    pure (sAll, apply sAll tv)

  EAssign l r -> do
    -- Guard: only mutable bindings may be assigned to
    case locVal l of
      EVar x -> case M.lookup x env of
        Just (_, Immutable) ->
          throwError (TypeError (Just sp) (ImmutableAssign x)
            [NoteHelp ("consider making `" <> x <> "` mutable: `let mut " <> x <> " = ...`")])
        _ -> pure ()
      _ -> pure ()  -- member/index assigns are permitted
    (s1, tl) <- inferExpr env l
    (s2, tr) <- inferExpr (apply s1 env) r
    s3 <- unify (apply s2 tl) tr
    let sAll = compose s3 (compose s2 s1)
    pure (sAll, apply sAll tr)

  EUnary op e -> do
    (s, t) <- inferExpr env e
    case op of
      Neg -> do
        s' <- unify t TIntT
        pure (compose s' s, TIntT)
      Not -> do
        s' <- unify t TBoolT
        pure (compose s' s, TBoolT)

  EBinary op a b -> do
    (s1, ta) <- inferExpr env a
    (s2, tb) <- inferExpr (apply s1 env) b
    let s12 = compose s2 s1
    case op of
      Add -> addOp ta tb s12
      Sub -> numNum ta tb s12 TIntT
      Mul -> numNum ta tb s12 TIntT
      Div -> numNum ta tb s12 TIntT
      Mod -> numNum ta tb s12 TIntT
      Lt -> ordBool ta tb s12
      Lte -> ordBool ta tb s12
      Gt -> ordBool ta tb s12
      Gte -> ordBool ta tb s12
      Eq -> eqBool ta tb s12
      Neq -> eqBool ta tb s12
      And -> boolBool ta tb s12
      Or -> boolBool ta tb s12
    where
      numNum ta tb s ret = do
        sA <- unify (apply s ta) TIntT
        sB <- unify (apply sA (apply s tb)) TIntT
        let sAll = compose sB (compose sA s)
        pure (sAll, ret)

      -- | The '+' operator supports both Int + Int -> Int and String + String -> String.
      -- If either operand is known to be a String, unify both as String.
      addOp ta tb s = do
        let ta' = apply s ta
            tb' = apply s tb
        case (ta', tb') of
          (TStringT, _) -> do
            sB <- unify (apply s tb) TStringT
            pure (compose sB s, TStringT)
          (_, TStringT) -> do
            sA <- unify (apply s ta) TStringT
            pure (compose sA s, TStringT)
          _ -> numNum ta tb s TIntT

      boolBool ta tb s = do
        sA <- unify (apply s ta) TBoolT
        sB <- unify (apply sA (apply s tb)) TBoolT
        pure (compose sB (compose sA s), TBoolT)

      ordBool ta tb s = do
        sA <- unify (apply s ta) TIntT
        sB <- unify (apply sA (apply s tb)) TIntT
        pure (compose sB (compose sA s), TBoolT)

      eqBool ta tb s = do
        sA <- unify (apply s ta) (apply s tb)
        pure (compose sA s, TBoolT)

  EIfExpr c t f -> do
    (s1, tc) <- inferExpr env c
    sBool <- unify tc TBoolT
    let s1' = compose sBool s1
    (s2, tt) <- inferExpr (apply s1' env) t
    (s3, tf) <- inferExpr (apply s2 (apply s1' env)) f
    s4 <- unify (apply s3 tt) tf
    let sAll = compose s4 (compose s3 (compose s2 s1'))
    pure (sAll, apply sAll tf)

  -- | Variant constructor: EnumName::VariantName(args)
  EVariant enumName varName args -> do
    (realEnumName, tyParams, variants) <- resolveEnumByName enumName
    case lookup varName [(vn, vf) | VariantSig vn vf <- variants] of
      Nothing -> throwSpanned (UnknownVariant realEnumName varName) []
      Just fieldSurfTypes -> do
        when (length args /= length fieldSurfTypes) $
          throwSpanned (VariantArityMismatch realEnumName varName
            (length fieldSurfTypes) (length args)) []
        -- Generate fresh type variables for the enum's type parameters
        freshTyArgs <- mapM (const fresh) tyParams
        let substMap = M.fromList (zip tyParams (map toSurfaceType freshTyArgs))
            instFields = map (substSurfaceType substMap) fieldSurfTypes
        -- Infer each argument and unify with the instantiated field type
        (sArgs, _) <- foldM (\(sAcc, idx) (arg, surfTy) -> do
          (sA, tA) <- inferExpr (apply sAcc env) arg
          iTy <- fromSurfaceType surfTy
          sU <- unify (apply sA tA) (apply sA iTy)
          let sAll = compose sU (compose sA sAcc)
          pure (sAll, idx + 1)
          ) (emptySubst, 0 :: Int) (zip args instFields)
        let resultType = TCon realEnumName (map (apply sArgs) freshTyArgs)
        pure (sArgs, resultType)

  -- | Match expression: match (scrutinee) { arms }
  EMatch scrut arms -> do
    (sScrut, tScrut) <- inferExpr env scrut
    -- Determine the enum being matched
    let tScrutResolved = apply sScrut tScrut
    (enumName, tyParams, variants, tyArgs) <-
      resolveEnumType tScrutResolved
    -- Build substitution from enum type params to concrete type args
    let tyArgSurface = map toSurfaceType tyArgs
        paramSubst = M.fromList (zip tyParams tyArgSurface)
    -- Infer each arm
    retTv <- fresh
    (sFinal, coveredVariants) <- foldM (\(sAcc, covered) (MatchArm pat body) -> do
      case pat of
        PWild -> do
          (sBody, tBody) <- inferExpr (apply sAcc env) body
          sRet <- unify (apply sBody tBody) (apply sBody (apply sAcc retTv))
          let sAll = compose sRet (compose sBody sAcc)
          -- Wildcard covers all remaining variants
          let allNames = S.fromList [vn | VariantSig vn _ <- variants]
          pure (sAll, S.union covered allNames)
        PVariant pEnum pVar bindings -> do
          (realPEnum, _, _) <- resolveEnumByName pEnum
          when (realPEnum /= enumName) $
            throwSpanned (OtherError ("pattern matches on `" <> pEnum
              <> "` but scrutinee is of type `" <> enumName <> "`")) []
          case lookup pVar [(vn, vf) | VariantSig vn vf <- variants] of
            Nothing -> throwSpanned (UnknownVariant enumName pVar) []
            Just fieldSurfTypes -> do
              when (length bindings /= length fieldSurfTypes) $
                throwSpanned (VariantArityMismatch enumName pVar
                  (length fieldSurfTypes) (length bindings)) []
              -- Instantiate field types
              let instFields = map (substSurfaceType paramSubst) fieldSurfTypes
              fieldITypes <- mapM fromSurfaceType instFields
              -- Extend environment with pattern bindings
              let envWithBindings = foldl
                    (\e (n, t) -> M.insert n (Forall [] (apply sAcc t), Immutable) e)
                    (apply sAcc env)
                    (zip bindings fieldITypes)
              (sBody, tBody) <- inferExpr envWithBindings body
              sRet <- unify (apply sBody tBody) (apply sBody (apply sAcc retTv))
              let sAll = compose sRet (compose sBody sAcc)
              pure (sAll, S.insert pVar covered)
      ) (sScrut, S.empty) arms
    -- Exhaustiveness check
    let allVariantNames = S.fromList [vn | VariantSig vn _ <- variants]
        missing = S.toList (S.difference allVariantNames coveredVariants)
    unless (null missing) $
      throwSpanned (NonExhaustiveMatch enumName missing) []
    pure (sFinal, apply sFinal retTv)

inferList :: TypeEnv -> [LExpr] -> Infer (Subst, [IType])
inferList env = foldM step (emptySubst, [])
  where
    step (sAcc, ts) e = do
      (s, t) <- inferExpr (apply sAcc env) e
      let sAll = compose s sAcc
      pure (sAll, ts <> [apply sAll t])

inferArgList :: TypeEnv -> [Arg] -> Infer (Subst, [IType])
inferArgList env args = inferList env [e | Arg e <- args]

inferStmt :: TypeEnv -> LStmt -> Infer (Subst, TypeEnv)
inferStmt env (Located sp stmt) = withCurrentSpan sp $ case stmt of
  SExpr e -> do
    (s, _) <- inferExpr env e
    pure (s, apply s env)

  SLet mut name mTy rhs -> do
    when (M.member name env) $
      throwError (TypeError (Just sp) (DuplicateBinding name) [])

    (s1, tRhs) <- inferExpr env rhs
    s2 <- case mTy of
      Nothing -> pure emptySubst
      Just ann -> do
        tAnn <- fromSurfaceType ann
        unify (apply s1 tRhs) tAnn
    let sAll = compose s2 s1
        env' = apply sAll env
        tFinal = apply sAll tRhs
        sch = generalize env' tFinal
    pure (sAll, M.insert name (sch, mut) env')

  SReturn _ ->
    -- Statement-level return checking can be made function-context-sensitive later.
    pure (emptySubst, env)

  SIf cond th el -> do
    (s1, tCond) <- inferExpr env cond
    sCond <- unify tCond TBoolT
    let sBase = compose sCond s1
    (sTh, envTh) <- inferBlock (apply sBase env) th
    (sEl, envEl) <- case el of
      Nothing -> pure (emptySubst, apply sTh envTh)
      Just b -> inferBlock (apply sTh envTh) b
    let sAll = compose sEl (compose sTh sBase)
        envMerged = envEl
    pure (sAll, envMerged)

  SWhile cond body -> do
    (s1, tCond) <- inferExpr env cond
    s2 <- unify tCond TBoolT
    (s3, env') <- inferBlock (apply (compose s2 s1) env) body
    pure (compose s3 (compose s2 s1), env')

  SBlock b -> inferBlock env b

  STypeDecl name params body -> do
    decls <- gets typeDecls
    when (M.member name decls) $
      throwError (TypeError (Just sp) (DuplicateType name) [])
    modify' (\s -> s { typeDecls = M.insert name (params, body) decls })
    pure (emptySubst, env)

  SEnum name tyParams variants -> do
    enums <- gets enumDecls
    when (M.member name enums) $
      throwError (TypeError (Just sp) (DuplicateType name) [])
    -- Register the enum
    let vsigs = [VariantSig vn vf | Variant vn vf <- variants]
    modify' (\s -> s { enumDecls = M.insert name (tyParams, vsigs) enums })
    -- Inject constructor functions into the type environment
    envOut <- foldM (\envAcc (Variant vn vfields) -> do
      -- Generate fresh type variables for the enum's type params
      freshTyArgs <- mapM (const fresh) tyParams
      let qualName = name <> "::" <> vn
          enumIType = TCon name freshTyArgs
      if null vfields
        then do
          -- Unit variant: just the enum type (polymorphic)
          let envFtv = S.unions [ftv sch | (sch, _) <- M.elems envAcc]
              vars = S.toList (ftv enumIType `S.difference` envFtv)
          pure (M.insert qualName (Forall vars enumIType, Immutable) envAcc)
        else do
          -- Variant with fields: function type
          let substMap = M.fromList (zip tyParams (map toSurfaceType freshTyArgs))
              instFields = map (substSurfaceType substMap) vfields
          fieldITypes <- mapM fromSurfaceType instFields
          let funTy = TFunT fieldITypes enumIType
              envFtv = S.unions [ftv sch | (sch, _) <- M.elems envAcc]
              vars = S.toList (ftv funTy `S.difference` envFtv)
          pure (M.insert qualName (Forall vars funTy, Immutable) envAcc)
      ) env variants
    pure (emptySubst, envOut)

  -- | 'export' is purely a resolver-level visibility marker; typechecking
  -- delegates straight through to the wrapped declaration.
  SExport inner -> inferStmt env inner

  -- | By the time a flattened program reaches the typechecker, the module
  -- resolver has already consumed every 'SImport' and replaced it with
  -- forwarding declarations. A surviving 'SImport' means the resolver was
  -- bypassed (e.g. a nested import, or 'inferProgram' called directly).
  SImport _ path ->
    throwSpanned (OtherError
      ("cannot resolve import of `" <> path <> "` outside the module resolver")) []

  SFun name tyParams params mRet body -> do
    -- Create fresh type variables for declared type parameters (e.g., <Msg>)
    -- and temporarily register them so fromSurfaceType resolves TApp "Msg" []
    -- to the same TV throughout the function's type annotations.
    freshTyVars <- mapM (const fresh) tyParams
    let newBinds = M.fromList (zip tyParams freshTyVars)
    oldBinds <- gets tyVarBinds
    modify' (\s -> s { tyVarBinds = M.union newBinds (tyVarBinds s) })

    paramTypes <- mapM (\(Param _ mt) -> maybe fresh fromSurfaceType mt) params
    retType <- maybe fresh fromSurfaceType mRet
    let funType = TFunT paramTypes retType
        envRec = M.insert name (Forall [] funType, Immutable) env
        envParams =
          foldl
            (\e (Param n _, t) -> M.insert n (Forall [] t, Immutable) e)
            envRec
            (zip params paramTypes)

    (sBody, _) <- inferBlockWithRet (Just retType) envParams body

    -- Restore tyVarBinds
    modify' (\s -> s { tyVarBinds = oldBinds })

    let funTypeFinal = apply sBody funType
        envApplied = apply sBody env
        sch = generalize envApplied funTypeFinal
        envOut = M.insert name (sch, Immutable) envApplied

    pure (sBody, envOut)

inferStmtWithRet :: Maybe IType -> TypeEnv -> LStmt -> Infer (Subst, TypeEnv)
inferStmtWithRet mRet env (Located sp stmt) = withCurrentSpan sp $ case stmt of
  SReturn me ->
    case (mRet, me) of
      (Nothing, _) ->
        -- top-level return (or unsupported context)
        pure (emptySubst, env)
      (Just rt, Nothing) -> do
        s <- unify rt TNullT
        pure (s, apply s env)
      (Just rt, Just e) -> do
        (s1, te) <- inferExpr env e
        s2 <- unify (apply s1 te) (apply s1 rt)
        let sAll = compose s2 s1
        pure (sAll, apply sAll env)

  SBlock b ->
    inferBlockWithRet mRet env b

  SIf cond th el -> do
    (s1, tCond) <- inferExpr env cond
    sCond <- unify tCond TBoolT
    let sBase = compose sCond s1
    (sTh, envTh) <- inferBlockWithRet mRet (apply sBase env) th
    (sEl, envEl) <- case el of
      Nothing -> pure (emptySubst, envTh)
      Just b -> inferBlockWithRet mRet envTh b
    let sAll = compose sEl (compose sTh sBase)
    pure (sAll, apply sAll envEl)

  SWhile cond body -> do
    (s1, tCond) <- inferExpr env cond
    s2 <- unify tCond TBoolT
    (s3, env') <- inferBlockWithRet mRet (apply (compose s2 s1) env) body
    pure (compose s3 (compose s2 s1), env')

  -- defer to existing behavior for others
  other -> inferStmt env (Located sp other)


inferBlockWithRet :: Maybe IType -> TypeEnv -> Block -> Infer (Subst, TypeEnv)
inferBlockWithRet mRet env (Block ss) =
  foldM step (emptySubst, env) ss
  where
    step (sAcc, eAcc) st = do
      (s, e) <- inferStmtWithRet mRet (apply sAcc eAcc) st
      pure (compose s sAcc, e)

inferBlock :: TypeEnv -> Block -> Infer (Subst, TypeEnv)
inferBlock env (Block ss) = foldM step (emptySubst, env) ss
  where
    step (sAcc, eAcc) st = do
      (s, e) <- inferStmt (apply sAcc eAcc) st
      pure (compose s sAcc, e)

inferProgram :: Program -> Either TypeError TypeEnv
inferProgram (Program stmts) = runInfer $ do
  (_, envOut) <- foldM step (emptySubst, preludeEnv) stmts
  pure envOut
  where
    step (sAcc, eAcc) st = do
      (s, e) <- inferStmt (apply sAcc eAcc) st
      pure (compose s sAcc, e)

inferExprInEmpty :: Expr -> Either TypeError IType
inferExprInEmpty e = runInfer $ do
  let le = Located dummySpan e
  (s, t) <- inferExpr preludeEnv le
  pure (apply s t)

preludeEnv :: TypeEnv
preludeEnv =
  M.fromList
    [ ("print", (Forall [0] (TFunT [TV 0] TNullT), Immutable)),
      ("toString", (Forall [1] (TFunT [TV 1] TStringT), Immutable))
    ]
