{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.Env.RecordParser
-- Description: Type class that provides parsers for records.
--
-- This module provides a type class 'RecordParser' that provides parsers for
-- records. The parsers are used to parse environment variables into records
-- based on their string representation.
--
-- Unlike a plain 'Either'-based approach, the generic implementation uses an
-- internal 'Validation' applicative so that every field is attempted and all
-- failures are collected into a single 'ParseError'.
module Data.Env.RecordParser (
  RecordParser (..),
) where

import Data.Env.ParseError
import Data.Env.TypeParser
import Data.Map ( Map )
import Data.Map qualified as M
import GHC.Generics

-- | Type class for validating environment schemas.
class RecordParser a where
  -- | Parse a record from a map of field-name → raw-string values.
  -- Returns a 'ParseError' listing *all* fields that failed, not just the
  -- first one.
  parseRecord :: Map String String -> Either ParseError a

  -- | Parse a record, discarding any error message on failure.
  parseRecord' :: Map String String -> Maybe a
  parseRecord' env = case parseRecord env of
    Right val -> Just val
    Left _    -> Nothing

instance (Generic a, GRecordParser (Rep a)) => RecordParser a where
  parseRecord env = validationToEither (to <$> gParseRecord env)


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
class GRecordParser f where
  gParseRecord :: Map String String -> Validation ParseError (f p)

-- | Handle metadata (wrapping fields in 'GHC.Generics.M1')
instance GRecordParser f => GRecordParser (M1 D c f) where
  gParseRecord env = M1 <$> gParseRecord env

-- | Handle metadata (wrapping fields in 'GHC.Generics.M1')
instance GRecordParser f => GRecordParser (M1 C c f) where
  gParseRecord env = M1 <$> gParseRecord env

-- | Handle multiple fields in a record — uses 'Validation' so both sides are
-- always evaluated, accumulating all failures.
instance (GRecordParser f, GRecordParser g) => GRecordParser (f :*: g) where
  gParseRecord env = (:*:) <$> gParseRecord env <*> gParseRecord env

-- | Handle an individual field — wraps any 'TypeParser' failure in a
-- 'FieldError' keyed by the Haskell field name.
instance (TypeParser a, Selector s) => GRecordParser (M1 S s (K1 i a)) where
  gParseRecord env =
    let key    = selName (undefined :: M1 S s (K1 i a) p)
        result = case M.lookup key env of
          Nothing -> parseMissing @a
          Just "" -> parseMissing @a
          Just v  -> parseType v
    in  M1 . K1 <$> case result of
          Left msg  -> VFailure $ ParseError [FieldError { errField = key, errMessage = msg }]
          Right val -> VSuccess val
