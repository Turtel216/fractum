{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module graph resolution ("linking") for the import/export system.
--
-- Compilation of a multi-file program proceeds in two phases, both driven
-- from a single entry file:
--
-- * __Discovery__ (IO): recursively reads and parses every transitively
-- imported file, resolving relative import paths, memoizing already-seen
-- modules, and detecting import cycles.
--
-- * __Link__ (pure): walks the discovered modules in dependency order and
-- flattens them into a single 'Program'. Top-level declaration names that
-- would otherwise collide across modules are renamed; each 'SImport' is
-- replaced with a small forwarding declaration under its local alias,
-- pointing at the final name of whatever it imports.
--
-- The output of 'resolveEntry' is an ordinary, single-module-looking
-- 'Program' that the existing (module-agnostic) typechecker, desugarer, and
-- emitter consume completely unchanged.
module Resolver (resolveEntry) where

import Ast
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Diagnostic (CompileError (..))
import Parser (parseProgram)
import System.Directory (doesFileExist)
import System.FilePath (normalise, takeDirectory, takeExtension, (<.>), (</>))
import Typecheck (TypeError (..), TypeErrorKind (..))

-- | A single parsed module: its normalised file path and top-level
-- statements.
data ModuleUnit = ModuleUnit FilePath [LStmt]

data DiscoverState = DiscoverState
  { dsVisited :: M.Map FilePath ModuleUnit,
    -- | dependency order: deps before dependents
    dsOrder :: [FilePath],
    dsSources :: M.Map FilePath Text
  }

-- | 'ExceptT' wraps 'StateT' (rather than the other way around) so that a
-- thrown error does not discard the sources read so far: 'resolveEntry'
-- wants those available even on failure, to render a proper snippet for
-- errors like 'ModuleNotFound' or 'CircularImport'.
--
-- The error type is a 'CompileError' rather than a 'TypeError' because
-- discovery both parses and resolves: a syntax error in an imported module
-- has to travel out of here with its own diagnostics intact.
type Discover a = ExceptT CompileError (StateT DiscoverState IO) a

-- | Resolve an import path relative to the file that wrote it. The
-- extension defaults to @.tjs@ when omitted.
resolveImportPath :: FilePath -> Text -> FilePath
resolveImportPath importingFile rawPath =
  let dir = takeDirectory importingFile
      p = T.unpack rawPath
      withExt = if null (takeExtension p) then p <.> "fr" else p
   in normalise (dir </> withExt)

-- | Depth-first discovery of the module graph starting at 'path'. 'stack' is
-- the chain of modules currently being discovered (deepest last), used for
-- cycle detection.
discoverModule :: [FilePath] -> FilePath -> Maybe Span -> Discover ()
discoverModule stack path mImportSpan
  | path `elem` stack =
      let cycleChain = dropWhile (/= path) (reverse stack) ++ [path]
       in throwError (TypeFailure (TypeError mImportSpan (CircularImport (map T.pack cycleChain)) []))
  | otherwise = do
      st <- get
      if M.member path (dsVisited st)
        then pure () -- already fully discovered elsewhere in the graph
        else do
          exists <- liftIO (doesFileExist path)
          if not exists
            then throwError (TypeFailure (TypeError mImportSpan (ModuleNotFound (T.pack path)) []))
            else do
              src <- liftIO (TIO.readFile path)
              modify' (\s -> s {dsSources = M.insert path src (dsSources s)})
              case parseProgram path src of
                Left perrs -> throwError (ParseFailure perrs)
                Right (Program stmts) -> do
                  let stack' = path : stack
                  forM_ stmts $ \(Located sp stmt) -> case stmt of
                    SImport _ rawPath ->
                      discoverModule stack' (resolveImportPath path rawPath) (Just sp)
                    _ -> pure ()
                  modify'
                    ( \s ->
                        s
                          { dsVisited = M.insert path (ModuleUnit path stmts) (dsVisited s),
                            dsOrder = dsOrder s ++ [path]
                          }
                    )

-- | Where a module-local declared name ends up after collision-renaming,
-- plus (for type/enum declarations) its original type parameters, needed to
-- build an arity-correct forwarding alias for importers.
data ExportEntry = ExportEntry {eeFinalName :: Name, eeTyParams :: [Name]}

data ModuleExports = ModuleExports
  { meValues :: M.Map Name ExportEntry,
    meTypes :: M.Map Name ExportEntry
  }

emptyExports :: ModuleExports
emptyExports = ModuleExports M.empty M.empty

data LinkState = LinkState
  { lsClaimedValues :: S.Set Name,
    lsClaimedTypes :: S.Set Name,
    lsExports :: M.Map FilePath ModuleExports
  }

-- | The namespace a top-level declaration lives in: values (@let@/@function@)
-- and types (@type@/@enum@) are tracked independently, mirroring the
-- separate 'Typecheck.TypeEnv' vs. @typeDecls@/@enumDecls@ namespaces.
data NameKind = ValNS | TypeNS

-- | The name (and, for type/enum decls, type parameters) a top-level
-- statement declares, looking through 'SExport'.
declOf :: Stmt -> Maybe (NameKind, Name, [Name])
declOf (SLet _ n _ _) = Just (ValNS, n, [])
declOf (SFun n _ _ _ _) = Just (ValNS, n, [])
declOf (STypeDecl n ps _) = Just (TypeNS, n, ps)
declOf (SEnum n ps _) = Just (TypeNS, n, ps)
declOf (SExport (Located _ inner)) = declOf inner
declOf _ = Nothing

-- | Pick a name not already in the claimed set, suffixing with an
-- incrementing counter on collision.
freshName :: S.Set Name -> Name -> Name
freshName claimed n
  | n `S.notMember` claimed = n
  | otherwise = go (2 :: Int)
  where
    go i =
      let cand = n <> "_" <> T.pack (show i)
       in if cand `S.member` claimed then go (i + 1) else cand

-- | Compute the rename maps (old -> final) for a module's own top-level
-- declarations, threading the running claimed-name sets forward for the
-- next module.
--
-- Collisions are only ever checked against 'claimedVals0'/'claimedTys0' —
-- names claimed by *other* (earlier-processed) modules — never against
-- names this same module is itself in the middle of declaring. A module
-- that (illegally) declares the same top-level name twice must keep both
-- occurrences identical so the existing duplicate-binding/duplicate-type
-- checks in 'Typecheck.inferStmt' still catch it; silently renaming one
-- of them apart would hide the error instead of reporting it.
renamePlan ::
  S.Set Name ->
  S.Set Name ->
  [LStmt] ->
  (M.Map Name Name, M.Map Name Name, S.Set Name, S.Set Name)
renamePlan claimedVals0 claimedTys0 stmts =
  foldl step (M.empty, M.empty, claimedVals0, claimedTys0) [s | Located _ s <- stmts]
  where
    step acc@(vRen, tRen, claimedVals, claimedTys) stmt = case declOf stmt of
      Nothing -> acc
      Just (ValNS, n, _)
        | M.member n vRen -> acc
        | otherwise ->
            let n' = freshName claimedVals0 n
             in (M.insert n n' vRen, tRen, S.insert n' claimedVals, claimedTys)
      Just (TypeNS, n, _)
        | M.member n tRen -> acc
        | otherwise ->
            let n' = freshName claimedTys0 n
             in (vRen, M.insert n n' tRen, claimedVals, S.insert n' claimedTys)

-- | Apply a module's own value/type rename maps to its statements.
--
-- Top-level declaration sites are renamed directly (they are exactly what
-- the plan is about), and the renamed value name stays visible to every
-- later top-level statement and to the declaration's own body (so
-- recursive calls resolve correctly). Nested declarations (inside a
-- function/if/while/match body) are never renamed themselves — they were
-- never part of the collision plan — but they do locally shadow the
-- rewrite of any later reference with the same spelling, exactly like
-- ordinary lexical scoping.
renameModule :: M.Map Name Name -> M.Map Name Name -> [LStmt] -> [LStmt]
renameModule valRen tyRen = map renameTop
  where
    renameTop :: LStmt -> LStmt
    renameTop (Located sp stmt) = Located sp (renameTopStmt stmt)

    renameTopStmt :: Stmt -> Stmt
    renameTopStmt stmt = case stmt of
      SLet mut n mty e ->
        SLet mut (M.findWithDefault n n valRen) (renameType <$> mty) (goExpr valRen e)
      SFun n tyParams params mret body ->
        let bodyEnv = foldr (M.delete . paramName) valRen params
         in SFun
              (M.findWithDefault n n valRen)
              tyParams
              (map renameParam params)
              (renameType <$> mret)
              (goBlock bodyEnv body)
      STypeDecl n ps body ->
        STypeDecl (M.findWithDefault n n tyRen) ps (renameType body)
      SEnum n ps variants ->
        SEnum (M.findWithDefault n n tyRen) ps (map renameVariant variants)
      SExport (Located isp inner) ->
        SExport (Located isp (renameTopStmt inner))
      SImport {} -> stmt
      other -> fst (goStmt valRen other)

    -- Nested (block-local) traversal: never renames a declaration site,
    -- only rewrites 'EVar' references, shadowing 'env' whenever a
    -- more-local binding reuses an outer renamed name.
    goStmts :: M.Map Name Name -> [LStmt] -> [LStmt]
    goStmts _ [] = []
    goStmts env (Located sp s : rest) =
      let (s', env') = goStmt env s
       in Located sp s' : goStmts env' rest

    goStmt :: M.Map Name Name -> Stmt -> (Stmt, M.Map Name Name)
    goStmt env stmt = case stmt of
      SLet mut n mty e ->
        (SLet mut n (renameType <$> mty) (goExpr env e), M.delete n env)
      SFun n tyParams params mret body ->
        let bodyEnv = foldr (M.delete . paramName) (M.delete n env) params
            body' = goBlock bodyEnv body
         in (SFun n tyParams (map renameParam params) (renameType <$> mret) body', M.delete n env)
      SReturn me -> (SReturn (goExpr env <$> me), env)
      SIf c th el -> (SIf (goExpr env c) (goBlock env th) (goBlock env <$> el), env)
      SWhile c b -> (SWhile (goExpr env c) (goBlock env b), env)
      SExpr e -> (SExpr (goExpr env e), env)
      SBlock b -> (SBlock (goBlock env b), env)
      STypeDecl n ps body -> (STypeDecl n ps (renameType body), env)
      SEnum n ps variants -> (SEnum n ps (map renameVariant variants), env)
      SExport (Located isp inner) ->
        let (inner', env') = goStmt env inner
         in (SExport (Located isp inner'), env')
      SImport {} -> (stmt, env)

    goBlock :: M.Map Name Name -> Block -> Block
    goBlock env (Block ss) = Block (goStmts env ss)

    goExpr :: M.Map Name Name -> LExpr -> LExpr
    goExpr env (Located sp e) = Located sp (goExprInner env e)

    goExprInner :: M.Map Name Name -> Expr -> Expr
    goExprInner env expr = case expr of
      EVar x -> EVar (M.findWithDefault x x env)
      ELit _ -> expr
      ELam params mret body ->
        let bodyEnv = foldr (M.delete . paramName) env params
         in ELam (map renameParam params) (renameType <$> mret) (goExpr bodyEnv body)
      ECall f args -> ECall (goExpr env f) (map (goArg env) args)
      EMember o f -> EMember (goExpr env o) f
      EIndex a i -> EIndex (goExpr env a) (goExpr env i)
      EObject fs -> EObject [(k, goExpr env v) | (k, v) <- fs]
      EArray es -> EArray (map (goExpr env) es)
      EAssign l r -> EAssign (goExpr env l) (goExpr env r)
      EUnary op e -> EUnary op (goExpr env e)
      EBinary op a b -> EBinary op (goExpr env a) (goExpr env b)
      EIfExpr c t f -> EIfExpr (goExpr env c) (goExpr env t) (goExpr env f)
      EParens e -> EParens (goExpr env e)
      EVariant en vn args ->
        EVariant (M.findWithDefault en en tyRen) vn (map (goExpr env) args)
      EMatch scrut arms -> EMatch (goExpr env scrut) (map (goArm env) arms)
      ECast e ty -> ECast (goExpr env e) (renameType ty)

    goArm :: M.Map Name Name -> MatchArm -> MatchArm
    goArm env (MatchArm pat body) = case pat of
      PWild -> MatchArm PWild (goExpr env body)
      PVariant en vn bindings ->
        let bodyEnv = foldr M.delete env bindings
         in MatchArm (PVariant (M.findWithDefault en en tyRen) vn bindings) (goExpr bodyEnv body)

    goArg :: M.Map Name Name -> Arg -> Arg
    goArg env (Arg e) = Arg (goExpr env e)

    renameParam :: Param -> Param
    renameParam (Param n mt) = Param n (renameType <$> mt)

    renameVariant :: Variant -> Variant
    renameVariant (Variant n fields) = Variant n (map renameType fields)

    -- Type/enum names have no local shadowing story in Fractum (there is no
    -- nested-type-declaration-shadows-an-outer-renamed-type scenario in
    -- practice), so the type rename map is applied uniformly rather than
    -- threaded through scopes the way the value map is.
    renameType :: Type -> Type
    renameType = \case
      TInt -> TInt
      TFloat -> TFloat
      TBool -> TBool
      TString -> TString
      TVar n -> TVar n
      TArray t -> TArray (renameType t)
      TObject fs -> TObject [(k, renameType v) | (k, v) <- fs]
      TApp n args -> TApp (M.findWithDefault n n tyRen) (map renameType args)
      TFun as r -> TFun (map renameType as) (renameType r)

paramName :: Param -> Name
paramName (Param n _) = n

-- | Build a module's export table, keyed by the *original* (pre-rename)
-- source name, from its own (pre-rename) top-level statements and rename
-- maps.
buildExports :: M.Map Name Name -> M.Map Name Name -> [LStmt] -> ModuleExports
buildExports valRen tyRen stmts =
  foldl step emptyExports [s | Located _ (SExport (Located _ s)) <- stmts]
  where
    step acc s = case declOf s of
      Nothing -> acc
      Just (ValNS, n, _) ->
        acc {meValues = M.insert n (ExportEntry (M.findWithDefault n n valRen) []) (meValues acc)}
      Just (TypeNS, n, ps) ->
        acc {meTypes = M.insert n (ExportEntry (M.findWithDefault n n tyRen) ps) (meTypes acc)}

-- | Resolve one module's own 'SImport' statements against already-linked
-- dependencies' export tables, producing forwarding declarations to prepend
-- under each item's local alias.
resolveImports :: M.Map FilePath ModuleExports -> FilePath -> [LStmt] -> Either TypeError [LStmt]
resolveImports exportsByModule thisPath stmts =
  concatMap maybeToList . concat <$> mapM forImport [(sp, s) | Located sp s@(SImport {}) <- stmts]
  where
    forImport (sp, SImport items rawPath) = do
      let depPath = resolveImportPath thisPath rawPath
          exports = M.findWithDefault emptyExports depPath exportsByModule
      mapM (forItem sp rawPath exports) items
    forImport (_, _) = pure []

    maybeToList Nothing = []
    maybeToList (Just x) = [x]

    -- \| A forwarding declaration is only needed when the local alias
    -- differs from the dependency's final name — when they're the same,
    -- the dependency's own (already-flattened, earlier-in-order)
    -- declaration is already directly usable under that exact name, and
    -- adding another declaration for it would just be a duplicate.
    forItem :: Span -> Text -> ModuleExports -> ImportItem -> Either TypeError (Maybe LStmt)
    forItem sp rawPath exports (ImportItem name mAlias) =
      let alias = fromMaybe name mAlias
       in case M.lookup name (meValues exports) of
            Just entry
              | alias == eeFinalName entry -> pure Nothing
              | otherwise ->
                  pure . Just $
                    Located
                      dummySpan
                      (SLet Immutable alias Nothing (Located dummySpan (EVar (eeFinalName entry))))
            Nothing -> case M.lookup name (meTypes exports) of
              Just entry
                | alias == eeFinalName entry -> pure Nothing
                | otherwise ->
                    let ps = eeTyParams entry
                     in pure . Just $ Located dummySpan (STypeDecl alias ps (TApp (eeFinalName entry) (map TVar ps)))
              Nothing -> Left (TypeError (Just sp) (UnboundImport name rawPath) [])

-- | Link one already-discovered module into the flattened program, updating
-- the running link state for the next module.
linkModule :: LinkState -> ModuleUnit -> Either TypeError ([LStmt], LinkState)
linkModule st (ModuleUnit path stmts) = do
  let (valRen, tyRen, claimedVals', claimedTys') =
        renamePlan (lsClaimedValues st) (lsClaimedTypes st) stmts
      exports = buildExports valRen tyRen stmts
      renamed = renameModule valRen tyRen stmts
      nonImports = [ls | ls@(Located _ s) <- renamed, not (isImport s)]
  forwarding <- resolveImports (lsExports st) path stmts
  let st' =
        st
          { lsClaimedValues = claimedVals',
            lsClaimedTypes = claimedTys',
            lsExports = M.insert path exports (lsExports st)
          }
  pure (forwarding ++ nonImports, st')
  where
    isImport (SImport {}) = True
    isImport _ = False

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Resolve the full module graph starting at 'entryFile'. Returns the
-- flattened 'Program' ready for 'Typecheck.inferProgram' (or the first
-- error hit) together with every source file successfully loaded, whether
-- or not resolution ultimately succeeded — so a diagnostic for e.g.
-- 'ModuleNotFound' can still render a snippet from the importing file.
resolveEntry :: FilePath -> IO (Either CompileError Program, M.Map FilePath Text)
resolveEntry entryFile = do
  let entryPath = normalise entryFile
  (discovered, ds) <-
    runStateT (runExceptT (discoverModule [] entryPath Nothing)) (DiscoverState M.empty [] M.empty)
  let linked = case discovered of
        Left err -> Left err
        Right () ->
          let modules = [dsVisited ds M.! p | p <- dsOrder ds]
           in case foldLink (LinkState S.empty S.empty M.empty) modules of
                Left err -> Left (TypeFailure err)
                Right (flattened, _) -> Right (Program flattened)
  pure (linked, dsSources ds)
  where
    foldLink :: LinkState -> [ModuleUnit] -> Either TypeError ([LStmt], LinkState)
    foldLink st [] = pure ([], st)
    foldLink st (m : ms) = do
      (stmts, st') <- linkModule st m
      (rest, stF) <- foldLink st' ms
      pure (stmts ++ rest, stF)
