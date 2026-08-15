{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Typecheck
-- Description : Semantic analysis, environment tracking, and type inference engine.
-- Stability   : experimental
--
-- This module implements a robust, pass-based typechecker based on an extended
-- Hindley-Milner (Algorithm W) type inference algorithm. It transforms an untyped
-- surface AST ('Ast.Program') into heavily validated semantic constructs, producing
-- a final type environment or short-circuiting with rich, span-aware errors.
--
-- This top-level module is a thin facade: it re-exports the public API and
-- provides the two pipeline entry points, 'inferProgram' and
-- 'inferExprInEmpty'. The implementation is split across:
--
-- * "Typecheck.Types" — the internal type representation ('Typecheck.Types.IType'),
--   type schemes, and substitutions.
-- * "Typecheck.Error" — 'TypeError' and its diagnostic payload.
-- * "Typecheck.Infer" — the 'Typecheck.Infer.Infer' monad itself: fresh type
--   variables, unification, and let-generalization. Unification folds each
--   new unifier directly into the monad's state rather than threading and
--   composing an explicit 'Typecheck.Types.Subst' through every function's
--   return value.
-- * "Typecheck.Surface" — conversion between the surface 'Ast.Type' syntax
--   and the internal 'Typecheck.Types.IType', plus enum declaration lookups.
-- * "Typecheck.Expr" and "Typecheck.Stmt" — the inference rules themselves.
--
-- Mutability is tracked alongside every binding's scheme in the type
-- environment; 'Ast.EAssign' explicitly guards against reassigning immutable
-- bindings, emitting a contextual help note when violated.
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
import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import Typecheck.Error
import Typecheck.Expr (inferExpr)
import Typecheck.Infer (runInfer, zonk)
import Typecheck.Stmt (inferStmt)
import Typecheck.Types

-- | Main pipeline entry point. Processes a sequence of top-level statements
-- against the prelude, populating declarations and producing a final
-- exported 'TypeEnv'.
inferProgram :: Program -> Either TypeError TypeEnv
inferProgram (Program stmts) = runInfer (foldM inferStmt preludeEnv stmts)

-- | Utility for testing. Infers the type of an isolated expression against a
-- base prelude environment.
inferExprInEmpty :: Expr -> Either TypeError IType
inferExprInEmpty e = runInfer $ do
  t <- inferExpr preludeEnv (Located dummySpan e)
  zonk t

preludeEnv :: TypeEnv
preludeEnv =
  M.fromList
    [ ("print", (Forall [0] (TFunT [TV 0] TNullT), Immutable)),
      ("toString", (Forall [1] (TFunT [TV 1] TStringT), Immutable)),
      -- Boots an the application via the JS runtime's
      -- TypedJS.app (see runtime/fractum_runtime.js). Left weakly typed
      -- like print/toString above; the config's init/update/view fields
      -- are still fully checked wherever they are defined and used.
      ("app", (Forall [2] (TFunT [TV 2] TNullT), Immutable))
    ]
