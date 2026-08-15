{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Typecheck.Stmt
-- Description : Type inference for statements, blocks, and declarations.
module Typecheck.Stmt
  ( inferStmt,
    inferBlock,
  )
where

import Ast
import Control.Monad (foldM, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify')
import qualified Data.Map.Strict as M
import Typecheck.Error
import Typecheck.Expr (inferExpr)
import Typecheck.Infer
import Typecheck.Surface
import Typecheck.Types

inferStmt :: TypeEnv -> LStmt -> Infer TypeEnv
inferStmt env (Located sp stmt) = withCurrentSpan sp $ case stmt of
  SExpr e -> do
    _ <- inferExpr env e
    pure env
  SLet mut name mTy rhs -> do
    when (M.member name env) $
      throwError (TypeError (Just sp) (DuplicateBinding name) [])
    tRhs <- inferExpr env rhs
    case mTy of
      Nothing -> pure ()
      Just ann -> do
        tAnn <- fromSurfaceType ann
        unify tRhs tAnn
    sch <- generalize env tRhs
    pure (M.insert name (sch, mut) env)
  SReturn _ ->
    -- Statement-level return checking can be made function-context-sensitive later.
    pure env
  SIf cond th el -> do
    tCond <- inferExpr env cond
    unify tCond TBoolT
    envTh <- inferBlock env th
    case el of
      Nothing -> pure envTh
      Just b -> inferBlock envTh b
  SWhile cond body -> do
    tCond <- inferExpr env cond
    unify tCond TBoolT
    inferBlock env body
  SBlock b -> inferBlock env b
  STypeDecl name params body -> do
    decls <- gets typeDecls
    when (M.member name decls) $
      throwError (TypeError (Just sp) (DuplicateType name) [])
    modify' (\s -> s {typeDecls = M.insert name (params, body) decls})
    pure env
  SEnum name tyParams variants -> do
    enums <- gets enumDecls
    when (M.member name enums) $
      throwError (TypeError (Just sp) (DuplicateType name) [])
    -- Register the enum
    let vsigs = [VariantSig vn vf | Variant vn vf <- variants]
    modify' (\s -> s {enumDecls = M.insert name (tyParams, vsigs) enums})
    -- Inject constructor functions into the type environment
    foldM registerVariant env variants
    where
      registerVariant envAcc (Variant vn vfields) = do
        -- Generate fresh type variables for the enum's type params
        freshTyArgs <- mapM (const fresh) tyParams
        let qualName = name <> "::" <> vn
            enumIType = TCon name freshTyArgs
        if null vfields
          then do
            -- Unit variant: just the enum type (polymorphic)
            sch <- generalize envAcc enumIType
            pure (M.insert qualName (sch, Immutable) envAcc)
          else do
            -- Variant with fields: function type
            let substMap = M.fromList (zip tyParams (map toSurfaceType freshTyArgs))
                instFields = map (substSurfaceType substMap) vfields
            fieldITypes <- mapM fromSurfaceType instFields
            let funTy = TFunT fieldITypes enumIType
            sch <- generalize envAcc funTy
            pure (M.insert qualName (sch, Immutable) envAcc)

  -- \| 'export' is purely a resolver-level visibility marker; typechecking
  -- delegates straight through to the wrapped declaration.
  SExport inner -> inferStmt env inner
  -- \| By the time a flattened program reaches the typechecker, the module
  -- resolver has already consumed every 'SImport' and replaced it with
  -- forwarding declarations. A surviving 'SImport' means the resolver was
  -- bypassed (e.g. a nested import, or 'inferProgram' called directly).
  SImport _ path ->
    throwSpanned
      ( OtherError
          ("cannot resolve import of `" <> path <> "` outside the module resolver")
      )
      []
  SFun name tyParams params mRet body -> do
    -- Create fresh type variables for declared type parameters (e.g., <Msg>)
    -- and temporarily register them so fromSurfaceType resolves TApp "Msg" []
    -- to the same TV throughout the function's type annotations.
    freshTyVars <- mapM (const fresh) tyParams
    let newBinds = M.fromList (zip tyParams freshTyVars)
    oldBinds <- gets tyVarBinds
    modify' (\s -> s {tyVarBinds = M.union newBinds (tyVarBinds s)})

    paramTypes <- mapM (\(Param _ mt) -> maybe fresh fromSurfaceType mt) params
    retType <- maybe fresh fromSurfaceType mRet
    let funType = TFunT paramTypes retType
        envRec = M.insert name (Forall [] funType, Immutable) env
        envParams =
          foldl
            (\e (Param n _, t) -> M.insert n (Forall [] t, Immutable) e)
            envRec
            (zip params paramTypes)

    _ <- inferBlockWithRet (Just retType) envParams body

    -- Restore tyVarBinds
    modify' (\s -> s {tyVarBinds = oldBinds})

    funTypeFinal <- zonk funType
    sch <- generalize env funTypeFinal
    pure (M.insert name (sch, Immutable) env)

inferStmtWithRet :: Maybe IType -> TypeEnv -> LStmt -> Infer TypeEnv
inferStmtWithRet mRet env (Located sp stmt) = withCurrentSpan sp $ case stmt of
  SReturn me ->
    case (mRet, me) of
      (Nothing, _) ->
        -- top-level return (or unsupported context)
        pure env
      (Just rt, Nothing) -> do
        unify rt TNullT
        pure env
      (Just rt, Just e) -> do
        te <- inferExpr env e
        unify te rt
        pure env
  SBlock b ->
    inferBlockWithRet mRet env b
  SIf cond th el -> do
    tCond <- inferExpr env cond
    unify tCond TBoolT
    envTh <- inferBlockWithRet mRet env th
    case el of
      Nothing -> pure envTh
      Just b -> inferBlockWithRet mRet envTh b
  SWhile cond body -> do
    tCond <- inferExpr env cond
    unify tCond TBoolT
    inferBlockWithRet mRet env body
  -- defer to existing behavior for others
  other -> inferStmt env (Located sp other)

inferBlockWithRet :: Maybe IType -> TypeEnv -> Block -> Infer TypeEnv
inferBlockWithRet mRet env (Block ss) = foldM (inferStmtWithRet mRet) env ss

inferBlock :: TypeEnv -> Block -> Infer TypeEnv
inferBlock env (Block ss) = foldM inferStmt env ss
