-- |
-- Module      : Diagnostic.Types
-- Description : The phase-independent vocabulary every compiler error is reported in.
--
-- Both the parser ("Parser.Error") and the typechecker ("Typecheck.Error")
-- produce their own richly structured error values. Neither of them knows how
-- a diagnostic is painted on a terminal; instead each one explains how it
-- collapses into the small, uniform 'Diagnostic' record defined here, and
-- "Diagnostic" renders that single shape.
--
-- Keeping the shape in its own leaf module (it depends on nothing but "Ast")
-- is what lets the renderer sit *above* both phases without either phase
-- having to depend on the other.
module Diagnostic.Types
  ( Note (..),
    Diagnostic (..),
    ToDiagnostic (..),
    tshow,
  )
where

import Ast (Span)
import Data.Text (Text)
import qualified Data.Text as T

-- | Additional context attached beneath an error's source snippet.
data Note
  = -- | plain @note:@ line
    NoteText Text
  | -- | @help:@ suggestion
    NoteHelp Text
  | -- | secondary source location with message
    NoteSpan Span Text
  deriving (Eq, Show)

-- | A fully elaborated error report, ready to be rendered.
--
-- Every field is already in its final, user-facing form: by the time an error
-- becomes a 'Diagnostic' there is no interpretation left to do, only layout.
data Diagnostic = Diagnostic
  { -- | stable error code, e.g. @E0001@ or @P0001@
    diagCode :: !Text,
    -- | short title shown on the header line
    diagTitle :: !Text,
    -- | primary source location, if the error has one
    diagSpan :: !(Maybe Span),
    -- | message shown under the caret underline
    diagDetail :: !Text,
    -- | secondary explanations
    diagNotes :: ![Note]
  }
  deriving (Eq, Show)

-- | Errors that can be presented to the user as a 'Diagnostic'.
--
-- Laws:
--
-- * /Totality/: @toDiagnostic@ is defined for every value of @e@; an error
--   that cannot be described is not an error the user can be shown.
--
-- * /Location preservation/: if @e@ carries a primary source span, then
--   @diagSpan (toDiagnostic e)@ is @Just@ that same span. A renderer is
--   free to omit a location, but never to invent or move one.
--
-- * /Code stability/: @diagCode@ depends only on the /kind/ of error, never
--   on the particular names or types inside it, so that codes stay
--   greppable across releases.
class ToDiagnostic e where
  toDiagnostic :: e -> Diagnostic

-- | 'show' straight into 'Text', for splicing numbers into messages.
tshow :: (Show a) => a -> Text
tshow = T.pack . show
