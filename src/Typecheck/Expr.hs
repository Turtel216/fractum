{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Typecheck.Expr
-- Description : Type inference for expressions.
module Typecheck.Expr
  ( inferExpr,
    inferList,
    inferArgList,
  )
where

import Ast
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Typecheck.Error
import Typecheck.Infer
import Typecheck.Surface
import Typecheck.Types

inferExpr :: TypeEnv -> LExpr -> Infer IType
inferExpr env (Located sp expr) = withCurrentSpan sp $ case expr of
  EVar x ->
    case M.lookup x env of
      Nothing -> throwError (TypeError (Just sp) (UnboundVariable x) [])
      Just (sch, _) -> instantiate sch
  ELit l -> case l of
    LInt _ -> pure TIntT
    LBool _ -> pure TBoolT
    LString _ -> pure TStringT
    LNull -> pure TNullT
  EParens e -> inferExpr env e
  EArray es -> do
    tv <- fresh
    ts <- inferList env es
    mapM_ (unify tv) ts
    zonk (TArrayT tv)
  EObject fs -> do
    typed <- mapM (\(k, e) -> (k,) <$> inferExpr env e) fs
    pure (TObjectT (M.fromList typed))
  ELam params mRet body -> do
    (env', ptys) <- foldM addParam (env, []) params
    tBody <- inferExpr env' body
    case mRet of
      Nothing -> pure ()
      Just rt -> do
        rt' <- fromSurfaceType rt
        unify tBody rt'
    ptys' <- mapM zonk ptys
    tBody' <- zonk tBody
    pure (TFunT ptys' tBody')
    where
      addParam (e, ts) (Param n mt) = do
        t <- maybe fresh fromSurfaceType mt
        pure (M.insert n (Forall [] t, Immutable) e, ts <> [t])
  ECall fn args -> do
    tFn <- inferExpr env fn
    argTs <- inferArgList env args
    ret <- fresh
    unify (TFunT argTs ret) tFn
    zonk ret
  EMember obj field -> do
    tObj <- zonk =<< inferExpr env obj
    case tObj of
      TObjectT fs ->
        case M.lookup field fs of
          Just tf -> pure tf
          Nothing -> throwError (TypeError (Just sp) (MissingField field) [])
      -- fallback for unknown/non-concrete object terms
      _ -> do
        tv <- fresh
        unify tObj (TObjectT (M.singleton field tv))
        zonk tv
  EIndex arr ix -> do
    tArr <- inferExpr env arr
    tIx <- inferExpr env ix
    unify tIx TIntT
    tv <- fresh
    unify tArr (TArrayT tv)
    zonk tv
  EAssign l r -> do
    -- Guard: only mutable bindings may be assigned to
    case locVal l of
      EVar x -> case M.lookup x env of
        Just (_, Immutable) ->
          throwError
            ( TypeError
                (Just sp)
                (ImmutableAssign x)
                [NoteHelp ("consider making `" <> x <> "` mutable: `let mut " <> x <> " = ...`")]
            )
        _ -> pure ()
      _ -> pure () -- member/index assigns are permitted
    tl <- inferExpr env l
    tr <- inferExpr env r
    unify tl tr
    zonk tr
  EUnary op e -> do
    t <- inferExpr env e
    case op of
      Neg -> unify t TIntT >> pure TIntT
      Not -> unify t TBoolT >> pure TBoolT
  EBinary op a b -> do
    ta <- inferExpr env a
    tb <- inferExpr env b
    case op of
      Add -> addOp ta tb
      Sub -> numNum ta tb
      Mul -> numNum ta tb
      Div -> numNum ta tb
      Mod -> numNum ta tb
      Lt -> ordBool ta tb
      Lte -> ordBool ta tb
      Gt -> ordBool ta tb
      Gte -> ordBool ta tb
      Eq -> eqBool ta tb
      Neq -> eqBool ta tb
      And -> boolBool ta tb
      Or -> boolBool ta tb
    where
      numNum ta tb = unify ta TIntT >> unify tb TIntT >> pure TIntT

      -- \| The '+' operator supports both Int + Int -> Int and String + String -> String.
      -- If either operand is known to be a String, unify both as String.
      addOp ta tb = do
        ta' <- zonk ta
        tb' <- zonk tb
        case (ta', tb') of
          (TStringT, _) -> unify tb TStringT >> pure TStringT
          (_, TStringT) -> unify ta TStringT >> pure TStringT
          _ -> numNum ta tb

      boolBool ta tb = unify ta TBoolT >> unify tb TBoolT >> pure TBoolT

      ordBool ta tb = unify ta TIntT >> unify tb TIntT >> pure TBoolT

      eqBool ta tb = unify ta tb >> pure TBoolT
  EIfExpr c t f -> do
    tc <- inferExpr env c
    unify tc TBoolT
    tt <- inferExpr env t
    tf <- inferExpr env f
    unify tt tf
    zonk tf

  -- \| Variant constructor: EnumName::VariantName(args)
  EVariant enumName varName args -> do
    (realEnumName, tyParams, variants) <- resolveEnumByName enumName
    case lookup varName [(vn, vf) | VariantSig vn vf <- variants] of
      Nothing -> throwSpanned (UnknownVariant realEnumName varName) []
      Just fieldSurfTypes -> do
        when (length args /= length fieldSurfTypes) $
          throwSpanned
            ( VariantArityMismatch
                realEnumName
                varName
                (length fieldSurfTypes)
                (length args)
            )
            []
        -- Generate fresh type variables for the enum's type parameters
        freshTyArgs <- mapM (const fresh) tyParams
        let substMap = M.fromList (zip tyParams (map toSurfaceType freshTyArgs))
            instFields = map (substSurfaceType substMap) fieldSurfTypes
        -- Infer each argument and unify with the instantiated field type
        forM_ (zip args instFields) $ \(arg, surfTy) -> do
          tA <- inferExpr env arg
          iTy <- fromSurfaceType surfTy
          unify tA iTy
        resultArgs <- mapM zonk freshTyArgs
        pure (TCon realEnumName resultArgs)

  -- \| Match expression: match (scrutinee) { arms }
  EMatch scrut arms -> do
    tScrut <- zonk =<< inferExpr env scrut
    -- Determine the enum being matched
    (enumName, tyParams, variants, tyArgs) <- resolveEnumType tScrut
    -- Build substitution from enum type params to concrete type args
    let tyArgSurface = map toSurfaceType tyArgs
        paramSubst = M.fromList (zip tyParams tyArgSurface)
    -- Infer each arm
    retTv <- fresh
    coveredVariants <-
      foldM
        ( \covered (MatchArm pat body) -> case pat of
            PWild -> do
              tBody <- inferExpr env body
              unify tBody retTv
              -- Wildcard covers all remaining variants
              let allNames = S.fromList [vn | VariantSig vn _ <- variants]
              pure (S.union covered allNames)
            PVariant pEnum pVar bindings -> do
              (realPEnum, _, _) <- resolveEnumByName pEnum
              when (realPEnum /= enumName) $
                throwSpanned
                  ( OtherError
                      ( "pattern matches on `"
                          <> pEnum
                          <> "` but scrutinee is of type `"
                          <> enumName
                          <> "`"
                      )
                  )
                  []
              case lookup pVar [(vn, vf) | VariantSig vn vf <- variants] of
                Nothing -> throwSpanned (UnknownVariant enumName pVar) []
                Just fieldSurfTypes -> do
                  when (length bindings /= length fieldSurfTypes) $
                    throwSpanned
                      ( VariantArityMismatch
                          enumName
                          pVar
                          (length fieldSurfTypes)
                          (length bindings)
                      )
                      []
                  -- Instantiate field types
                  let instFields = map (substSurfaceType paramSubst) fieldSurfTypes
                  fieldITypes <- mapM fromSurfaceType instFields
                  -- Extend environment with pattern bindings
                  let envWithBindings =
                        foldl
                          (\e (n, t) -> M.insert n (Forall [] t, Immutable) e)
                          env
                          (zip bindings fieldITypes)
                  tBody <- inferExpr envWithBindings body
                  unify tBody retTv
                  pure (S.insert pVar covered)
        )
        S.empty
        arms
    -- Exhaustiveness check
    let allVariantNames = S.fromList [vn | VariantSig vn _ <- variants]
        missing = S.toList (S.difference allVariantNames coveredVariants)
    unless (null missing) $
      throwSpanned (NonExhaustiveMatch enumName missing) []
    zonk retTv

inferList :: TypeEnv -> [LExpr] -> Infer [IType]
inferList env = mapM (inferExpr env)

inferArgList :: TypeEnv -> [Arg] -> Infer [IType]
inferArgList env args = inferList env [e | Arg e <- args]
