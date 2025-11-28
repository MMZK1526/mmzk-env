-- |
-- Module      : Data.Env
-- Description : Environment schema validation
--
-- This module provides functionality to validate environment variables against
-- a schema (a type that implements the `EnvSchema` class).
module Data.Env ( EnvSchema(..) ) where

import Control.Monad.IO.Class
import Data.Env.ExtractFields
import Data.Env.RecordParser

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
