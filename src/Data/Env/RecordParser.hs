{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.Env.RecordParser
-- Description: Type class that provides parsers for records.
--
-- This module provides a type class 'RecordParser' that provides parsers for
-- records. The parsers are used to parse environment variables into records
-- based on their string representation.
module Data.Env.RecordParser (
  RecordParser (..),
) where

import Data.Env.TypeParser
import Data.Map ( Map )
import Data.Map qualified as M
import Data.Maybe
import GHC.Generics

-- | Type class for validating environment schemas.
class RecordParser a where
  parseRecord :: Map String String -> Either String a

  -- | Parse a record, converting 'Either' to 'Maybe' and dropping any error messages.
  --
  -- This is a convenience function that calls 'parseRecord' and converts the result
  -- from 'Either String a' to 'Maybe a', discarding the error message on failure.
  parseRecord' :: Map String String -> Maybe a
  parseRecord' env = case parseRecord env of
    Right val -> Just val
    Left _    -> Nothing

instance (Generic a, GRecordParser (Rep a)) => RecordParser a where

  parseRecord :: (Generic a, GRecordParser (Rep a))
              => Map String String -> Either String a
  parseRecord env = to <$> gParseRecord env


--------------------------------------------------------------------------------
-- Generic instances
--------------------------------------------------------------------------------

-- | Generic validation class.
class GRecordParser f where
  gParseRecord :: Map String String -> Either String (f p)

-- | Handle metadata (wrapping fields in 'GHC.Generics.M1')
instance GRecordParser f => GRecordParser (M1 D c f) where
  gParseRecord :: Map String String -> Either String (M1 D c f p)
  gParseRecord env = M1 <$> gParseRecord env

-- | Handle metadata (wrapping fields in 'GHC.Generics.M1')
instance GRecordParser f => GRecordParser (M1 C c f) where
  gParseRecord :: Map String String -> Either String (M1 C c f p)
  gParseRecord env = M1 <$> gParseRecord env

-- | Handle multiple fields in a record
instance (GRecordParser f, GRecordParser g) => GRecordParser (f :*: g) where
  gParseRecord :: Map String String -> Either String ((f :*: g) p)
  gParseRecord env = (:*:) <$> gParseRecord env <*> gParseRecord env

-- | Handle individual fields
instance (TypeParser a, Selector s) => GRecordParser (M1 S s (K1 i a)) where
  gParseRecord :: Map String String -> Either String (M1 S s (K1 i a) p)
  gParseRecord env =
    let key = selName (undefined :: M1 S s (K1 i a) p)
    in  M1 . K1 <$> case parseType (fromMaybe "" $ M.lookup key env) of
        Left err  -> Left $ "Field " ++ show key ++ " parsing error:\n" ++ err
        Right val -> Right val
