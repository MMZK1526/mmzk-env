{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.Env.ExtractFields
-- Description: Type class for extracting fields from a record type.
--
-- This module provides a type class 'ExtractFields' that extracts field names
-- from a record type. It also provides functions to retrieve environment
-- variables based on these field names, with options for different naming
-- conventions.
module Data.Env.ExtractFields (
  ExtractFields,
  extractFields,
  getEnvRaw,
  getEnvRawLowerToUpperSnake,
  getEnvRawCamelCaseToUpperSnake,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Map ( Map )
import Data.Map qualified as M
import Data.Maybe
import Data.Proxy
import GHC.Generics
import System.Environment

-- | Type class for extracting field names from a record type.
class ExtractFields a where
  -- | Extract field names from a record type. It uses a 'Proxy' to avoid
  -- needing a value of type $a$, and we recommend using 'extractFields'
  -- instead.
  extractFields' :: Proxy a -> [String]

-- | Extract field names from a record type.
extractFields :: forall a. ExtractFields a => [String]
extractFields = extractFields' (Proxy :: Proxy a)
{-# INLINE extractFields #-}

-- | Retrieve environment variables based on field names, applying a mapping
-- function to the field names.
getEnvRaw :: forall a m. (MonadIO m, ExtractFields a)
          => (String -> String)
          -> m (Map String String)
getEnvRaw mapper = liftIO $ M.fromList <$> forM (extractFields @a) \field -> do
  value <- lookupEnv (mapper field)
  return (field, fromMaybe "" value)
{-# INLINE getEnvRaw #-}

-- | Retrieve environment variables based on field names, converting them to
-- upper case.
getEnvRawLowerToUpperSnake :: forall a m. (MonadIO m, ExtractFields a)
                           => m (Map String String)
getEnvRawLowerToUpperSnake = getEnvRaw @a (map toUpper)
{-# INLINE getEnvRawLowerToUpperSnake #-}

-- | Retrieve environment variables based on field names, converting them from
-- camel case (record field naming) to upper snake case (environment variable
-- naming).
getEnvRawCamelCaseToUpperSnake :: forall a m. (MonadIO m, ExtractFields a)
                               => m (Map String String)
getEnvRawCamelCaseToUpperSnake = getEnvRaw @a camelToUpperSnake
{-# INLINE getEnvRawCamelCaseToUpperSnake #-}


--------------------------------------------------------------------------------
-- Generic instances
--------------------------------------------------------------------------------

-- | Generic helper class for extracting field names from a generic
-- representation.
class GExtractFields f where
  gExtractFields :: Proxy f -> [String]

instance GExtractFields f => GExtractFields (M1 D c f) where
  gExtractFields :: Proxy (M1 D c f) -> [String]
  gExtractFields _ = gExtractFields (Proxy :: Proxy f)
  {-# INLINE gExtractFields #-}

instance GExtractFields f => GExtractFields (M1 C c f) where
  gExtractFields :: Proxy (M1 C c f) -> [String]
  gExtractFields _ = gExtractFields (Proxy :: Proxy f)
  {-# INLINE gExtractFields #-}

instance (GExtractFields f, GExtractFields g) => GExtractFields (f :*: g) where
  gExtractFields :: Proxy (f :*: g) -> [String]
  gExtractFields _ = gExtractFields (Proxy :: Proxy f)
                  ++ gExtractFields (Proxy :: Proxy g)
  {-# INLINE gExtractFields #-}

instance (Selector s) => GExtractFields (M1 S s (K1 i a)) where
  gExtractFields :: Proxy (M1 S s (K1 i a)) -> [String]
  gExtractFields _ = [selName (undefined :: M1 S s (K1 i a) p)]
  {-# INLINE gExtractFields #-}

instance (Generic a, GExtractFields (Rep a)) => ExtractFields a where
  extractFields' :: Proxy a -> [String]
  extractFields' _ = gExtractFields (Proxy :: Proxy (Rep a))
  {-# INLINE extractFields' #-}


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

camelToUpperSnake :: String -> String
camelToUpperSnake = go False
  where
    go _ [] = []
    go prevUpper (x:xs)
      | x == '_'  = '_' : go True xs
      | isUpper x =
          let rest = go True xs
          in  if prevUpper then toUpper x : rest else '_' : toUpper x : rest
      | otherwise = toUpper x : go False xs
