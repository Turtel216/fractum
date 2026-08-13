-- | Command line interface definition
module Cli where

import Options.Applicative

-- | CLI Options
data Options = Options
  { -- | Source file
    sourceFile :: [FilePath],
    -- | Enable optimizations
    optimizations :: Bool,
    -- | Output file emited by compiler
    outputFile :: Maybe FilePath,
    -- | Disable colored output
    disableColor :: Bool
  }
  deriving (Show)

-- | Parser for CLI options
optionsParser :: Parser Options
optionsParser =
  Options
    <$> some
      ( argument
          str
          ( metavar "SOURCE_FILES..."
              <> help "Source file to process"
          )
      )
    <*> switch
      ( long "opt"
          <> short 'O'
          <> help "Enable compiler optimizations"
      )
    <*> optional
      ( option
          str
          ( long "output"
              <> short 'o'
              <> help "Output file emited by Compiler[default out.js]"
          )
      )
    <*> fmap
      (== Just "never")
      ( optional
          ( option
              str
              ( long "color"
                  <> help "Disable coloured output (--color=never)"
              )
          )
      )

-- | Full parser with help text
optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Compile Fractum files to readable javascript"
        <> header "Fractum - type-safe Javascript dialect"
    )
