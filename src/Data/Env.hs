{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Data.Env
-- Description : Environment schema validation
--
-- This module provides functionality to validate environment variables against
-- a schema (a type that implements the `EnvSchema` class).
module Data.Env ( EnvSchema(..), EnvSchemaW(..) ) where

import Control.Monad.IO.Class
import Data.Env.ExtractFields
import Data.Env.RecordParser
import Data.Env.RecordParserW

-- | Type class for validating environment schemas.
class (ExtractFields a, RecordParser a) => EnvSchema a where
  -- | Validate the environment variables against the schema, transforming field
  -- names from camelCase to UPPER_SNAKE_CASE.
  validateEnv :: MonadIO m => m (Either String a)
  validateEnv = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecord envRaw

  -- | Validate the environment variables against the schema, allowing for a
  -- cusutom transformation function to be applied to the field names to match
  -- with the environment variable names.
  validateEnvWith :: MonadIO m => (String -> String) -> m (Either String a)
  validateEnvWith transform = do
    envRaw <- getEnvRaw @a transform
    return $ parseRecord envRaw

-- | Type class for validating environment schemas using witness types.
--
-- Uses the witness pattern to enhance parsing (e.g., default values) without
-- wrapping final values in newtypes. Use @Config \'Dec@ for parsing and get
-- @Config \'Res@ with unwrapped values as the result.
--
-- @
-- data Config c = Config
--   { psqlPort :: Column c (DefaultNum 5432 Word16) Word16
--   , dbName   :: Column c (Solo String) String }
--   deriving (Generic)
--
-- instance EnvSchemaW (Config \'Dec)
--
-- loadConfig :: IO (Either String (Config \'Res))
-- loadConfig = validateEnvW \@(Config \'Dec)
-- @
class (ExtractFields a, RecordParserW a) => EnvSchemaW a where
  -- | Validate environment variables, transforming field names from camelCase
  -- to UPPER_SNAKE_CASE. Returns the parsed configuration with unwrapped values.
  validateEnvW :: MonadIO m => m (Either String (RecordParsedType a))
  validateEnvW = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecordW @a envRaw

  -- | Validate environment variables with a custom field name transformation
  -- function. Returns the parsed configuration with unwrapped values.
  validateEnvWWith :: MonadIO m => (String -> String) -> m (Either String (RecordParsedType a))
  validateEnvWWith transform = do
    envRaw <- getEnvRaw @a transform
    return $ parseRecordW @a envRaw
