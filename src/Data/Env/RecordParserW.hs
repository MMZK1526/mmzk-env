{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module: Data.Env.RecordParserW
-- Description: Type class that provides parsers for records with witnesses.
--
-- This module provides a type class 'RecordParserW' that provides parsers for
-- records with witness types. The parsers are used to parse environment variables
-- into records based on their string representation, with the witness types
-- providing custom parsing behavior per field.
module Data.Env.RecordParserW (
  RecordParserW (..),
  ColumnType (..),
  Column,
  Di,
) where

import Data.Data
import Data.Env.TypeParserW
import Data.Kind
import Data.Map ( Map )
import Data.Map qualified as M
import Data.Maybe
import GHC.Generics

-- | Column type indicator for distinguishing between declaration and result types.
--
-- * 'Dec' - Declaration type containing both witness and value types
-- * 'Res' - Result type containing only value types
data ColumnType = Dec | Res
  deriving stock (Eq, Show)

-- | Type family that maps column types to their representation.
--
-- For 'Dec' (declaration) columns, the type is a pair of witness and value.
-- For 'Res' (result) columns, the type is just the value.
--
-- In application code, typically only 'Res is used to access the parsed record values,
-- while 'Dec is used for the schema definition during parsing.
type family Column (t :: ColumnType) (p :: Type) (a :: Type) where
  Column 'Dec p a = (p, a)
  Column 'Res p a = a

-- | Type class for parsing environment schemas with witness types.
class RecordParserW a where
  -- | The result type after parsing, which removes witness types.
  type RecordParsedType a

  -- | Parse a record from environment variables using witness types.
  parseRecordW :: Map String String -> Either String (RecordParsedType a)

  -- | Parse a record, converting 'Either' to 'Maybe' and dropping any error messages.
  --
  -- This is a convenience function that calls 'parseRecordW' and converts the result
  -- from 'Either String a' to 'Maybe a', discarding the error message on failure.
  parseRecordW' :: Map String String -> Maybe (RecordParsedType a)
  parseRecordW' env = case parseRecordW @a env of
    Right val -> Just val
    Left _    -> Nothing

instance
  ( Generic (a 'Dec)
  , GRecordParserW (Rep (a 'Dec))
  , Generic (a Res)
  , GRecordParsedType (Rep (a 'Dec)) () ~ Rep (a 'Res) ()
  ) => RecordParserW (a 'Dec) where
  type RecordParsedType (a 'Dec) = a 'Res

  parseRecordW :: Map String String -> Either String (RecordParsedType (a 'Dec))
  parseRecordW a = to <$> gParseRecord @(Rep (a 'Dec)) a


--------------------------------------------------------------------------------
-- Generic instances
--------------------------------------------------------------------------------

-- | Generic validation class.
class GRecordParserW f where
  type GRecordParsedType f :: k -> Type

  gParseRecord :: Map String String -> Either String ((GRecordParsedType f) r)

-- | Handle metadata (wrapping fields in `M1`)
instance GRecordParserW f => GRecordParserW (M1 D c f) where
  type GRecordParsedType (M1 D c f) = M1 D c (GRecordParsedType f)

  gParseRecord :: Map String String -> Either String (GRecordParsedType (M1 D c f) r)
  gParseRecord env = M1 <$> gParseRecord @f env

-- | Handle metadata (wrapping fields in `M1`)
instance GRecordParserW f => GRecordParserW (M1 C c f) where
  type GRecordParsedType (M1 C c f) = M1 C c (GRecordParsedType f)

  gParseRecord :: Map String String -> Either String (GRecordParsedType (M1 C c f) r)
  gParseRecord env = M1 <$> gParseRecord @f env

-- | Handle multiple fields in a record
instance (GRecordParserW f, GRecordParserW g) => GRecordParserW (f :*: g) where
  type GRecordParsedType (f :*: g) = GRecordParsedType f :*: GRecordParsedType g

  gParseRecord :: Map String String -> Either String ((GRecordParsedType f :*: GRecordParsedType g) p)
  gParseRecord env = (:*:) <$> gParseRecord @f env <*> gParseRecord @g env

-- | Handle individual fields
instance (TypeParserW p a, Selector s) => GRecordParserW (M1 S s (K1 i (p, a))) where
  type GRecordParsedType (M1 S s (K1 i (p, a))) = M1 S s (K1 i a)

  gParseRecord :: Map String String -> Either String (M1 S s (K1 i a) r)
  gParseRecord env =
    let key = selName (undefined :: M1 S s (K1 i a) p)
    in  M1 . K1 <$> case parseTypeW @p Proxy (fromMaybe "" $ M.lookup key env) of
        Left err  -> Left $ "Field " ++ show key ++ " parsing error:\n" ++ err
        Right val -> Right val

-- | Type alias for declaring fields with polymorphic witness types.
--
-- This is a convenience alias for 'Column c (f a) a', commonly used to declare
-- record fields with a witness type @f a@ and a result type @a@.
type Di f c a = Column c (f a) a
