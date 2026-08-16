module Driver where

import Ast (Program)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Desuger (lowerProgram)
import Diagnostic (CompileError (..), renderCompileError)
import Emit (emitProgram)
import Parser
import Resolver (resolveEntry)
import Typecheck

-- | Single-file compilation pipeline: parses exactly one file's text with no
-- import resolution. Used for programs with no 'import' statements.
compileSource :: Bool -> FilePath -> T.Text -> Either T.Text T.Text
compileSource disableColor file src =
  case parseProgram file src of
    Left perrs ->
      Left (renderCompileError disableColor srcs (ParseFailure perrs))
    Right prog ->
      compileProgram disableColor srcs prog
  where
    srcs = M.singleton file src

-- | Full compilation pipeline starting from an entry file: resolves the
-- whole import graph into a single flattened 'Program', then typechecks,
-- desugars, and emits it exactly as 'compileSource' would a single file.
compileFile :: Bool -> FilePath -> IO (Either T.Text T.Text)
compileFile disableColor entryFile = do
  (result, srcs) <- resolveEntry entryFile
  pure $ case result of
    Left cerr -> Left (renderCompileError disableColor srcs cerr)
    Right prog -> compileProgram disableColor srcs prog

-- | Typecheck, desugar, and emit an already-parsed (and, for multi-file
-- programs, already-linked) 'Program'.
compileProgram :: Bool -> M.Map FilePath T.Text -> Program -> Either T.Text T.Text
compileProgram disableColor srcs prog =
  case inferProgram prog of
    Left terr ->
      Left (renderCompileError disableColor srcs (TypeFailure terr))
    Right _ ->
      Right (emitProgram (lowerProgram prog))
