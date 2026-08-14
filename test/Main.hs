{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Driver (compileFile)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.Process.Typed (proc, readProcess_)
import Test.Tasty
import Test.Tasty.Golden

-- | Helper for the E2E test suite. Runs the compiler pipeline, resolving
-- any imports relative to 'inputFile'.
compileToJS :: FilePath -> IO (Either T.Text T.Text)
compileToJS inputFile = compileFile True inputFile

main :: IO ()
main = do
  -- Dynamically discover pass and fail test cases
  passTests <- discoverTests mkPassTestCase "test/e2e/pass"
  failTests <- discoverTests mkFailTestCase "test/e2e/fail"

  defaultMain $
    testGroup
      "Compiler E2E Tests"
      [ testGroup "Pass Cases" passTests,
        testGroup "Fail Cases" failTests
      ]

-- | Traverses the given directory and applies the specific test generator
discoverTests :: (FilePath -> IO TestTree) -> FilePath -> IO [TestTree]
discoverTests mkTest baseDir = do
  contents <- listDirectory baseDir
  let paths = map (baseDir </>) contents
  -- Keep only directories (e.g., "001-basic-arithmetic")
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
        goldenVsString "1. Code Generation" expectedJsFile $ do
          result <- compileToJS inputFile
          case result of
            Left err ->
              -- If a 'pass' test returns a compilation error, crash the test immediately.
              fail $ "Expected successful compilation, but got error:\n" ++ T.unpack err
            Right jsOutput -> do
              -- Save it to disk for Node to run, and for manual inspection
              T.writeFile actualJsFile jsOutput
              -- Convert Text -> Strict ByteString -> Lazy ByteString for tasty-golden
              return (BL.fromStrict $ TE.encodeUtf8 jsOutput),
        -- Test 2: Runtime Behavior
        goldenVsString "2. Runtime Behavior" expectedOutFile $ do
          result <- compileToJS inputFile
          case result of
            Left _ ->
              -- No need to print the error again here, Test 1 will catch it.
              fail "Compilation failed, cannot execute."
            Right jsOutput -> do
              -- Ensure the file exists for this concurrent thread
              T.writeFile actualJsFile jsOutput

              -- Invoke Node.js, execute the file, and capture BOTH streams
              -- TODO: Check if one of a list of runtimes(node, bun etc) and use that one
              let nodeProcess = proc "node" [actualJsFile]
              (stdout, stderr) <- readProcess_ nodeProcess

              return (stdout <> stderr)
      ]

-- | Constructs the golden tests for a single test directory in fail/
mkFailTestCase :: FilePath -> IO TestTree
mkFailTestCase dir = do
  let testName = takeFileName dir
      inputFile = dir </> "input.fr"
      expectedErrFile = dir </> "expected.stderr"

  return $ goldenVsString testName expectedErrFile $ do
    result <- compileToJS inputFile
    case result of
      Right _ ->
        fail "Expected compilation to fail with type errors, but it succeeded!"
      Left err -> do
        -- Normalize the error output before comparing or saving
        let normalizedErr = normalizeDiagnostics inputFile err
        return (BL.fromStrict $ TE.encodeUtf8 normalizedErr)

-- | Strips absolute/relative paths and environment-specific data from errors
normalizeDiagnostics :: FilePath -> T.Text -> T.Text
normalizeDiagnostics filePath err =
  -- Replace the literal file path with a generic "$FILE" token.
  -- You might also want to strip trailing whitespace or normalize Windows \r\n here.
  T.replace (T.pack filePath) "$FILE" err
