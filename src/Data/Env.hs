{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Data.Env
-- Description : Environment schema validation
--
-- This module provides functionality to validate environment variables against
-- a schema (a type that implements the `EnvSchema` class).
module Data.Env (
  EnvSchema (..),
  EnvSchemaW (..),
  ParseError (..),
  FieldError (..),
  renderParseError,
  renderFieldError,
) where

import Control.Monad.IO.Class
import Data.Env.ExtractFields
import Data.Env.ParseError
import Data.Env.RecordParser
import Data.Env.RecordParserW

-- | Type class for validating environment schemas.
class (ExtractFields a, RecordParser a) => EnvSchema a where
  -- | Validate the environment variables against the schema, transforming field
  -- names from camelCase to UPPER_SNAKE_CASE.
  validateEnv :: MonadIO m => m (Either ParseError a)
  validateEnv = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecord envRaw

  -- | Validate the environment variables against the schema, allowing for a
  -- custom transformation function to be applied to the field names to match
  -- with the environment variable names.
  validateEnvWith :: MonadIO m => (String -> String) -> m (Either ParseError a)
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
-- loadConfig :: IO (Either ParseError (Config \'Res))
-- loadConfig = validateEnvW \@(Config \'Dec)
-- @
class (ExtractFields a, RecordParserW a) => EnvSchemaW a where
  -- | Validate environment variables, transforming field names from camelCase
  -- to UPPER_SNAKE_CASE. Returns the parsed configuration with unwrapped values.
  --
  -- An underscore is inserted before any uppercase letter not immediately
  -- preceded by another uppercase letter or an underscore. Notable behaviours:
  --
  -- * Consecutive uppercase runs are not split: @myHTTPClient@ maps to
  --   @MY_HTTPCLIENT@, not @MY_HTTP_CLIENT@. Place the word boundary before
  --   the run to get a split: @myHttpClient@ → @MY_HTTP_CLIENT@.
  -- * A digit followed by an uppercase letter still inserts an underscore:
  --   @http2Client@ maps to @HTTP2_CLIENT@. Trailing digits pass through
  --   without one: @port8080@ maps to @PORT8080@.
  -- * A literal underscore in the field name is passed through unchanged and
  --   suppresses the auto-inserted separator: @my_host@ maps to @MY_HOST@,
  --   not @MY__HOST@.
  -- * A field name starting with an uppercase letter produces a leading
  --   underscore. Use 'validateEnvWWith' with a custom transform to avoid this.
  validateEnvW :: MonadIO m => m (Either ParseError (RecordParsedType a))
  validateEnvW = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecordW @a envRaw

  -- | Validate environment variables with a custom field name transformation
  -- function. Returns the parsed configuration with unwrapped values.
  validateEnvWWith :: MonadIO m => (String -> String) -> m (Either ParseError (RecordParsedType a))
  validateEnvWWith transform = do
    envRaw <- getEnvRaw @a transform
    return $ parseRecordW @a envRaw
