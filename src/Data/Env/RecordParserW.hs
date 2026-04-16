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
--
-- Like 'RecordParser', the generic implementation uses an internal 'Validation'
-- applicative so that all field failures are collected rather than
-- short-circuiting on the first one.
module Data.Env.RecordParserW (
  RecordParserW (..),
  ColumnType (..),
  Column,
  Di,
) where

import Data.Data
import Data.Env.ParseError
import Data.Env.TypeParserW
import Data.Kind
import Data.Map ( Map )
import Data.Map qualified as M
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
  -- Returns a 'ParseError' listing *all* fields that failed, not just the
  -- first one.
  parseRecordW :: Map String String -> Either ParseError (RecordParsedType a)

  -- | Parse a record, discarding any error message on failure.
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

  parseRecordW :: Map String String -> Either ParseError (RecordParsedType (a 'Dec))
  parseRecordW a = validationToEither (to <$> gParseRecord @(Rep (a 'Dec)) a)


--------------------------------------------------------------------------------
-- Internal Validation applicative for error accumulation
--------------------------------------------------------------------------------

-- | Like 'Either', but the 'Applicative' instance accumulates failures using
-- the 'Semigroup' on @e@ instead of short-circuiting.
data Validation e a = VFailure e | VSuccess a

instance Functor (Validation e) where
  fmap _ (VFailure e) = VFailure e
  fmap f (VSuccess a) = VSuccess (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = VSuccess
  VSuccess f  <*> VSuccess x  = VSuccess (f x)
  VFailure e1 <*> VFailure e2 = VFailure (e1 <> e2)
  VFailure e  <*> _           = VFailure e
  _           <*> VFailure e  = VFailure e

validationToEither :: Validation e a -> Either e a
validationToEither (VSuccess a) = Right a
validationToEither (VFailure e) = Left e


--------------------------------------------------------------------------------
-- Generic instances
--------------------------------------------------------------------------------

-- | Generic validation class.
class GRecordParserW f where
  type GRecordParsedType f :: k -> Type

  gParseRecord :: Map String String -> Validation ParseError ((GRecordParsedType f) r)

-- | Handle metadata (wrapping fields in 'GHC.Generics.M1')
instance GRecordParserW f => GRecordParserW (M1 D c f) where
  type GRecordParsedType (M1 D c f) = M1 D c (GRecordParsedType f)

  gParseRecord env = M1 <$> gParseRecord @f env

-- | Handle metadata (wrapping fields in 'GHC.Generics.M1')
instance GRecordParserW f => GRecordParserW (M1 C c f) where
  type GRecordParsedType (M1 C c f) = M1 C c (GRecordParsedType f)

  gParseRecord env = M1 <$> gParseRecord @f env

-- | Handle multiple fields in a record — uses 'Validation' so both sides are
-- always evaluated, accumulating all failures.
instance (GRecordParserW f, GRecordParserW g) => GRecordParserW (f :*: g) where
  type GRecordParsedType (f :*: g) = GRecordParsedType f :*: GRecordParsedType g

  gParseRecord env = (:*:) <$> gParseRecord @f env <*> gParseRecord @g env

-- | Handle an individual field — wraps any 'TypeParserW' failure in a
-- 'FieldError' keyed by the Haskell field name.
instance (TypeParserW p a, Selector s) => GRecordParserW (M1 S s (K1 i (p, a))) where
  type GRecordParsedType (M1 S s (K1 i (p, a))) = M1 S s (K1 i a)

  gParseRecord env =
    let key    = selName (undefined :: M1 S s (K1 i a) p)
        result = case M.lookup key env of
          Nothing -> parseMissingW @p Proxy
          Just "" -> parseMissingW @p Proxy
          Just v  -> parseTypeW @p Proxy v
    in  M1 . K1 <$> case result of
          Left msg  -> VFailure $ ParseError [FieldError { errField = key, errMessage = msg }]
          Right val -> VSuccess val

-- | Type alias for declaring fields with polymorphic witness types.
--
-- This is a convenience alias for 'Column c (f a) a', commonly used to declare
-- record fields with a witness type @f a@ and a result type @a@.
type Di f c a = Column c (f a) a
