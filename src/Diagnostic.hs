{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostic rendering for compiler errors.
--
-- Produces colour-annotated error messages with source-line snippets, caret
-- underlines, and contextual @note:@ / @help:@ messages.
--
-- The renderer is deliberately ignorant of which phase produced an error. Both
-- "Parser.Error" and "Typecheck.Error" explain how they collapse into a
-- 'Diagnostic' (see 'Diagnostic.Types.ToDiagnostic'), and everything below
-- works on that one shape — so a syntax error and a type error are laid out by
-- the same code and are indistinguishable in style.
module Diagnostic
  ( renderDiagnostic,
    renderDiagnostics,
    CompileError (..),
    renderCompileError,
    prettyType,
  )
where

import Ast (Pos (..), Span (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Diagnostic.Types (Diagnostic (..), Note (..), ToDiagnostic (..), tshow)
import Parser.Error (ParseError)
import Typecheck (TypeError)
import Typecheck.Pretty (prettyType)

-- | Any error that aborts compilation, whichever phase raised it.
--
-- Parsing reports every syntax error it found in one pass, so its failure case
-- is a list; the checker stops at the first type error.
data CompileError
  = ParseFailure [ParseError]
  | TypeFailure TypeError
  deriving (Eq, Show)

-- | Render a whole compilation failure.
renderCompileError :: Bool -> M.Map FilePath Text -> CompileError -> Text
renderCompileError noc srcs = \case
  ParseFailure errs -> renderDiagnostics noc srcs errs
  TypeFailure err -> renderDiagnostic noc srcs err

ansi :: Text -> Text -> Text
ansi code t = "\ESC[" <> code <> "m" <> t <> "\ESC[0m"

red :: Bool -> Text -> Text
red True = id
red False = ansi "1;31"

blue :: Bool -> Text -> Text
blue True = id
blue False = ansi "1;34"

bold :: Bool -> Text -> Text
bold True = id
bold False = ansi "1"

-- | Render the coloured header line:  @error[E0001]: type mismatch@
errorHeader :: Bool -> Diagnostic -> Text
errorHeader noc d =
  red noc ("error[" <> diagCode d <> "]") <> bold noc (": " <> diagTitle d)

-- | Render a source-line snippet with caret underline and message.
renderSnippet :: Bool -> Int -> Text -> Span -> Text -> Text
renderSnippet noc gw src (Span _ (Pos line col) (Pos endLine endCol)) msg =
  let srcLines = T.lines src
      lineStr = tshow line
      lineNum = T.justifyRight gw ' ' lineStr
      pad = T.replicate (gw + 1) " "
      emptyG = pad <> blue noc "|"
      srcG = blue noc (lineNum <> " |") <> " "
      ulG = pad <> blue noc "|" <> " "
   in if line >= 1 && line <= length srcLines
        then
          let srcLine = lineAt srcLines line
              lineLen = T.length srcLine
              startOff = max 0 (col - 1)
              caretLen
                | line == endLine && endCol > col = endCol - col
                | otherwise = max 1 (lineLen - startOff)
              safeLen = max 1 (min caretLen (lineLen - startOff + 1))
              spacing = T.replicate startOff " "
           in emptyG
                <> "\n"
                <> srcG
                <> srcLine
                <> "\n"
                <> ulG
                <> spacing
                <> red noc (T.replicate safeLen "^")
                <> " "
                <> red noc msg
                <> "\n"
                <> emptyG
                <> "\n"
        else -- Span outside source (e.g. dummy span)
          emptyG
            <> "\n"
            <> ulG
            <> red noc msg
            <> "\n"
            <> emptyG
            <> "\n"

-- | The 1-indexed source line, or empty when out of range. Total by
-- construction, unlike indexing into 'T.lines'.
lineAt :: [Text] -> Int -> Text
lineAt srcLines n = case drop (n - 1) srcLines of
  (l : _) -> l
  [] -> ""

renderNote :: Bool -> Int -> Note -> Text
renderNote noc gw note =
  let pad = T.replicate (gw + 1) " "
   in case note of
        NoteText txt -> pad <> blue noc "= " <> bold noc "note: " <> txt <> "\n"
        NoteHelp txt -> pad <> blue noc "= " <> bold noc "help: " <> txt <> "\n"
        NoteSpan _ txt -> pad <> blue noc "= " <> bold noc "note: " <> txt <> "\n"

-- | Render a complete, coloured diagnostic string for any compiler error.
--
-- Takes a map of every source file loaded during compilation (keyed by the
-- file path recorded on each 'Span') so that an error originating inside an
-- imported module renders that module's own file and source snippet, not
-- the entry module's.
renderDiagnostic :: (ToDiagnostic e) => Bool -> M.Map FilePath Text -> e -> Text
renderDiagnostic noc srcs err =
  header
    <> "\n"
    <> locationStr
    <> snippetStr
    <> notesStr
  where
    d = toDiagnostic err

    gutterW = case diagSpan d of
      Just (Span _ (Pos l _) _) -> max 1 (length (show l))
      Nothing -> 1

    header = errorHeader noc d

    locationStr = case diagSpan d of
      Nothing -> ""
      Just (Span fp (Pos l c) _) ->
        T.replicate gutterW " "
          <> blue noc "--> "
          <> T.pack fp
          <> ":"
          <> tshow l
          <> ":"
          <> tshow c
          <> "\n"

    snippetStr = case diagSpan d of
      Nothing ->
        let pad = T.replicate (gutterW + 1) " "
         in pad
              <> blue noc "|"
              <> "\n"
              <> pad
              <> blue noc "| "
              <> red noc (diagDetail d)
              <> "\n"
              <> pad
              <> blue noc "|"
              <> "\n"
      Just sp@(Span fp _ _) ->
        renderSnippet noc gutterW (M.findWithDefault "" fp srcs) sp (diagDetail d)

    notesStr = T.concat [renderNote noc gutterW n | n <- diagNotes d]

-- | Render several diagnostics, separated by a blank line.
renderDiagnostics :: (ToDiagnostic e) => Bool -> M.Map FilePath Text -> [e] -> Text
renderDiagnostics noc srcs = T.intercalate "\n" . map (renderDiagnostic noc srcs)
