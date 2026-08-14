{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostic rendering for type errors.
--
-- Produces colour-annotated error messages  with source-line snippets, caret underlines, and contextual
-- @note:@ / @help:@ messages.
module Diagnostic
  ( renderDiagnostic,
    prettyType,
  )
where

import Ast (Pos (..), Span (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Typecheck (IType (..), Note (..), TVarId, TypeError (..), TypeErrorKind (..))

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

-- | Render an 'IType' as a user-friendly string.
prettyType :: IType -> Text
prettyType = \case
  TV n -> prettyTVar n
  TIntT -> "Int"
  TBoolT -> "Bool"
  TStringT -> "String"
  TNullT -> "Null"
  TFunT [] r -> "() -> " <> prettyType r
  TFunT [a] r -> prettyTypeAtom a <> " -> " <> prettyType r
  TFunT as r -> "(" <> T.intercalate ", " (map prettyType as) <> ") -> " <> prettyType r
  TArrayT t -> "[" <> prettyType t <> "]"
  TObjectT fs ->
    "{ " <> T.intercalate ", " [k <> ": " <> prettyType v | (k, v) <- M.toList fs] <> " }"
  TCon n [] -> n
  TCon n ts -> n <> "<" <> T.intercalate ", " (map prettyType ts) <> ">"

-- | Parenthesise function types when they appear as arguments to another
-- function type.
prettyTypeAtom :: IType -> Text
prettyTypeAtom t@(TFunT {}) = "(" <> prettyType t <> ")"
prettyTypeAtom t = prettyType t

-- | Render a type variable id as a lowercase letter (a, b, …, z, a1, b1, …).
prettyTVar :: TVarId -> Text
prettyTVar n
  | n < 26 = T.singleton (toEnum (fromEnum 'a' + n))
  | otherwise =
      T.singleton (toEnum (fromEnum 'a' + (n `mod` 26)))
        <> tshow (n `div` 26)

-- | Stable error code for each error kind.
errorCode :: TypeErrorKind -> Text
errorCode = \case
  TypeMismatch {} -> "E0001"
  InfiniteType {} -> "E0002"
  UnboundVariable {} -> "E0003"
  MissingField {} -> "E0004"
  DuplicateBinding {} -> "E0005"
  ImmutableAssign {} -> "E0006"
  UnboundType {} -> "E0007"
  DuplicateType {} -> "E0008"
  TypeArityMismatch {} -> "E0009"
  NonExhaustiveMatch {} -> "E0010"
  UnknownVariant {} -> "E0011"
  UnknownEnum {} -> "E0012"
  VariantArityMismatch {} -> "E0013"
  ModuleNotFound {} -> "E0014"
  UnboundImport {} -> "E0015"
  CircularImport {} -> "E0016"
  OtherError {} -> "E0099"

-- | Short human-readable title for the error kind.
errorTitle :: TypeErrorKind -> Text
errorTitle = \case
  TypeMismatch {} -> "type mismatch"
  InfiniteType {} -> "infinite type"
  UnboundVariable {} -> "cannot find value in this scope"
  MissingField {} -> "missing field"
  DuplicateBinding {} -> "duplicate definition"
  ImmutableAssign {} -> "cannot assign to immutable variable"
  UnboundType {} -> "undefined type"
  DuplicateType {} -> "duplicate type definition"
  TypeArityMismatch {} -> "wrong number of type arguments"
  NonExhaustiveMatch {} -> "non-exhaustive match"
  UnknownVariant {} -> "unknown variant"
  UnknownEnum {} -> "unknown enum"
  VariantArityMismatch {} -> "wrong number of variant fields"
  ModuleNotFound {} -> "module not found"
  UnboundImport {} -> "no exported member"
  CircularImport {} -> "circular import"
  OtherError {} -> "type error"

-- | Detailed message shown under the source underline.
errorDetail :: TypeErrorKind -> Text
errorDetail = \case
  TypeMismatch t1 t2 ->
    "expected `" <> prettyType t2 <> "`, found `" <> prettyType t1 <> "`"
  InfiniteType v t ->
    "type variable `" <> prettyTVar v <> "` occurs in `" <> prettyType t <> "`"
  UnboundVariable _name ->
    "not found in this scope"
  MissingField name ->
    "no field `" <> name <> "` on this type"
  DuplicateBinding name ->
    "`" <> name <> "` is already defined in this scope"
  ImmutableAssign name ->
    "cannot assign to `" <> name <> "`"
  UnboundType name ->
    "type `" <> name <> "` is not defined"
  DuplicateType name ->
    "type `" <> name <> "` is already defined"
  TypeArityMismatch name expected got ->
    "`"
      <> name
      <> "` expects "
      <> tshow expected
      <> " type argument(s) but "
      <> tshow got
      <> " were given"
  NonExhaustiveMatch name missing ->
    "match on `" <> name <> "` is not exhaustive, missing: "
      <> T.intercalate ", " ["`" <> v <> "`" | v <- missing]
  UnknownVariant enumN varN ->
    "variant `" <> varN <> "` does not exist on enum `" <> enumN <> "`"
  UnknownEnum name ->
    "enum `" <> name <> "` is not defined"
  VariantArityMismatch enumN varN expected got ->
    "variant `"
      <> varN
      <> "` of enum `"
      <> enumN
      <> "` expects "
      <> tshow expected
      <> " field(s) but "
      <> tshow got
      <> " were given"
  ModuleNotFound path ->
    "cannot find module `" <> path <> "`"
  UnboundImport name path ->
    "module `" <> path <> "` has no exported member `" <> name <> "`"
  CircularImport cyclePath ->
    "modules form a cycle: " <> T.intercalate " -> " cyclePath
  OtherError msg -> msg

-- | Render the coloured header line:  @error[E0001]: type mismatch@
errorHeader :: Bool -> TypeErrorKind -> Text
errorHeader noc kind =
  red noc ("error[" <> errorCode kind <> "]") <> bold noc (": " <> errorTitle kind)

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
          let srcLine = srcLines !! (line - 1)
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

renderNote :: Bool -> Int -> Note -> Text
renderNote noc gw note =
  let pad = T.replicate (gw + 1) " "
   in case note of
        NoteText txt -> pad <> blue noc "= " <> bold noc "note: " <> txt <> "\n"
        NoteHelp txt -> pad <> blue noc "= " <> bold noc "help: " <> txt <> "\n"
        NoteSpan _ txt -> pad <> blue noc "= " <> bold noc "note: " <> txt <> "\n"

-- | Render a complete, coloured diagnostic string for a 'TypeError'.
--
-- Takes a map of every source file loaded during compilation (keyed by the
-- file path recorded on each 'Span') so that an error originating inside an
-- imported module renders that module's own file and source snippet, not
-- the entry module's.
renderDiagnostic :: Bool -> M.Map FilePath Text -> TypeError -> Text
renderDiagnostic noc srcs (TypeError mSpan kind notes) =
  header
    <> "\n"
    <> locationStr
    <> snippetStr
    <> notesStr
  where
    gutterW = case mSpan of
      Just (Span _ (Pos l _) _) -> max 1 (length (show l))
      Nothing -> 1

    header = errorHeader noc kind

    locationStr = case mSpan of
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

    snippetStr = case mSpan of
      Nothing ->
        let pad = T.replicate (gutterW + 1) " "
         in pad
              <> blue noc "|"
              <> "\n"
              <> pad
              <> blue noc "| "
              <> red noc (errorDetail kind)
              <> "\n"
              <> pad
              <> blue noc "|"
              <> "\n"
      Just sp@(Span fp _ _) ->
        renderSnippet noc gutterW (M.findWithDefault "" fp srcs) sp (errorDetail kind)

    notesStr = T.concat [renderNote noc gutterW n | n <- notes]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
