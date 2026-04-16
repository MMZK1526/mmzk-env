-- |
-- Module: Data.Env.ParseError
-- Description: Structured parse error types for environment variable validation.
--
-- Provides 'FieldError' (a single field's failure) and 'ParseError' (all
-- field failures from a record), plus rendering functions.
module Data.Env.ParseError (
  FieldError (..),
  ParseError (..),
  renderFieldError,
  renderParseError,
) where

import Data.List ( intercalate )

-- | A parse failure for a single record field.
data FieldError = FieldError
  { errField   :: String  -- ^ The Haskell record field name.
  , errMessage :: String  -- ^ What went wrong parsing the value.
  } deriving (Show, Eq)

-- | All field parse failures collected from a record.
--
-- Using a list (rather than short-circuiting on the first failure) means
-- every invalid field is reported in one pass.
newtype ParseError = ParseError { parseErrors :: [FieldError] }
  deriving (Show, Eq)

-- | 'ParseError' is a 'Semigroup' so failures from different fields can be
-- accumulated by the generic record parser.
instance Semigroup ParseError where
  ParseError a <> ParseError b = ParseError (a <> b)

instance Monoid ParseError where
  mempty = ParseError []

-- | Render a single field error: the field name on the first line, then the
-- detail message indented below.
--
-- @
-- port: invalid field
--   (line 1, column 1):
--   unexpected "n"
--   expected an integer
--   >not-a-number
--    ^
-- @
renderFieldError :: FieldError -> String
renderFieldError fe =
  fe.errField ++ ": invalid field\n" ++ indentMsg "  " fe.errMessage

-- | Render all parse errors as a human-readable string.
--
-- A single failure uses 'renderFieldError'; multiple failures are numbered.
renderParseError :: ParseError -> String
renderParseError (ParseError [])  = "no parse errors"
renderParseError (ParseError [e]) = renderFieldError e
renderParseError (ParseError es)  =
  show (length es) ++ " fields failed to parse:\n"
  ++ intercalate "\n" (zipWith renderNumbered [1 :: Int ..] es)
  where
    renderNumbered i e =
      "  " ++ show i ++ ". " ++ e.errField ++ ": invalid field\n"
      ++ indentMsg "     " e.errMessage

-- Indent every line of a (possibly multi-line) message by a fixed prefix.
indentMsg :: String -> String -> String
indentMsg prefix = intercalate "\n" . map (prefix ++) . lines
