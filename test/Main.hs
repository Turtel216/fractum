{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Driver (compileFile)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, takeFileName, (</>))
import System.Process.Typed (proc, readProcess_)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase)

main :: IO ()
main = do
  passTests <- discoverDirTests mkPassTestCase "test/e2e/pass"
  failTests <- discoverDirTests mkFailTestCase "test/e2e/fail"
  stdlibTests <- discoverCompileTests "stdlib"
  exampleTests <- discoverCompileTests "examples"

  defaultMain $
    testGroup
      "Compiler E2E Tests"
      [ testGroup "Pass Cases" passTests,
        testGroup "Fail Cases" failTests,
        testGroup "Stdlib Compiles" stdlibTests,
        testGroup "Examples Compile" exampleTests
      ]

--------------------------------------------------------------------------------
-- Compilation helpers, shared by every suite below.
--------------------------------------------------------------------------------

-- | Runs the compiler pipeline, resolving any imports relative to 'inputFile'.
compileToJS :: FilePath -> IO (Either T.Text T.Text)
compileToJS inputFile = compileFile True inputFile

-- | Compiles 'inputFile', immediately failing the enclosing test with the
-- diagnostic if compilation errors. Shared by every test case that expects
-- compilation to succeed.
expectCompileSuccess :: FilePath -> IO T.Text
expectCompileSuccess inputFile = do
  result <- compileToJS inputFile
  case result of
    Left err -> assertFailure $ "Expected successful compilation, but got error:\n" ++ T.unpack err
    Right jsOutput -> pure jsOutput

-- | Compiles 'inputFile' and writes the JS output to 'actualJsFile', both to
-- drive Node in runtime tests and to leave the output on disk for manual
-- inspection.
compileAndSave :: FilePath -> FilePath -> IO T.Text
compileAndSave inputFile actualJsFile = do
  jsOutput <- expectCompileSuccess inputFile
  TIO.writeFile actualJsFile jsOutput
  pure jsOutput

-- | Encodes compiler output as the lazy UTF-8 'ByteString' tasty-golden expects.
toGolden :: T.Text -> BL.ByteString
toGolden = BL.fromStrict . TE.encodeUtf8

--------------------------------------------------------------------------------
-- Pass / Fail suites: one test directory per case, each holding "input.fr"
-- plus golden files with the expected output.
--------------------------------------------------------------------------------

-- | Traverses 'baseDir' and applies the given test-case builder to every
-- immediate subdirectory (e.g. "001-basic-arithmetic").
discoverDirTests :: (FilePath -> IO TestTree) -> FilePath -> IO [TestTree]
discoverDirTests mkTest baseDir = do
  contents <- listDirectory baseDir
  let paths = map (baseDir </>) contents
  testDirs <- filterM doesDirectoryExist paths
  mapM mkTest testDirs

-- | Constructs the golden tests for a single test directory in pass/
mkPassTestCase :: FilePath -> IO TestTree
mkPassTestCase dir = do
  let testName = takeFileName dir
      inputFile = dir </> "input.fr"
      expectedJsFile = dir </> "expected.js"
      expectedOutFile = dir </> "expected.out"
      actualJsFile = dir </> "actual.js"

  return $
    testGroup
      testName
      [ -- Test 1: Code Generation
        goldenVsString "1. Code Generation" expectedJsFile $
          toGolden <$> compileAndSave inputFile actualJsFile,
        -- Test 2: Runtime Behavior
        goldenVsString "2. Runtime Behavior" expectedOutFile $ do
          _ <- compileAndSave inputFile actualJsFile

          -- Invoke Node.js, execute the file, and capture BOTH streams
          -- TODO: Check if one of a list of runtimes(node, bun etc) and use that one
          let nodeProcess = proc "node" [actualJsFile]
          (stdout, stderr) <- readProcess_ nodeProcess

          return (stdout <> stderr)
      ]

-- | Constructs the golden test for a single test directory in fail/
mkFailTestCase :: FilePath -> IO TestTree
mkFailTestCase dir = do
  let testName = takeFileName dir
      inputFile = dir </> "input.fr"
      expectedErrFile = dir </> "expected.stderr"

  return $ goldenVsString testName expectedErrFile $ do
    result <- compileToJS inputFile
    case result of
      Right _ ->
        assertFailure "Expected compilation to fail with type errors, but it succeeded!"
      Left err ->
        -- Normalize the error output before comparing or saving
        pure (toGolden (normalizeDiagnostics inputFile err))

-- | Strips absolute/relative paths and environment-specific data from errors
normalizeDiagnostics :: FilePath -> T.Text -> T.Text
normalizeDiagnostics filePath err =
  -- Replace the literal file path with a generic "$FILE" token.
  -- You might also want to strip trailing whitespace or normalize Windows \r\n here.
  T.replace (T.pack filePath) "$FILE" err

--------------------------------------------------------------------------------
-- Compile-only suites: every ".fr" file under a directory tree must compile,
-- with no expectations on the generated JS (stdlib/, examples/).
--------------------------------------------------------------------------------

-- | Recursively finds every ".fr" source file under a directory, ignoring
-- any other files (e.g. the ".html" files mixed into "examples/").
findSourceFiles :: FilePath -> IO [FilePath]
findSourceFiles baseDir = do
  contents <- listDirectory baseDir
  let paths = map (baseDir </>) contents
      sources = filter ((== ".fr") . takeExtension) paths
  subDirs <- filterM doesDirectoryExist paths
  nestedSources <- concat <$> mapM findSourceFiles subDirs
  return (sources ++ nestedSources)

-- | Discovers every source file under 'baseDir' (searched recursively) and
-- builds a test case asserting that it compiles without error. We only care
-- that compilation succeeds, not the generated JS, so these are plain
-- 'testCase' assertions rather than golden tests.
discoverCompileTests :: FilePath -> IO [TestTree]
discoverCompileTests baseDir = do
  sources <- findSourceFiles baseDir
  return $ map (mkCompileTestCase baseDir) sources

-- | Asserts that a single source file compiles successfully. The test name
-- is its path relative to 'baseDir', so files of the same name in different
-- subdirectories (e.g. multiple examples) remain distinguishable.
mkCompileTestCase :: FilePath -> FilePath -> TestTree
mkCompileTestCase baseDir inputFile =
  testCase (makeRelative baseDir inputFile) $
    void (expectCompileSuccess inputFile)
