{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.IO
import Driver
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  opts <- execParser optsInfo
  let file = head $ sourceFile opts
      outputF = fromMaybe "out.js" (outputFile opts)

  result <- compileFile (disableColor opts) file

  case result of
    Left err -> do
      -- Print compilation errors to stderr so they don't pollute stdout
      TIO.hPutStrLn stderr err
    Right js -> do
      TIO.writeFile outputF js
      putStrLn $ "Generated " ++ outputF
