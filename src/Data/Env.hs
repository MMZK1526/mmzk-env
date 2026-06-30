{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : Data.Env
Description : Environment-variable schema validation

Top-level interface.  'EnvSchemaW' is the primary class; 'EnvSchema' covers
the simpler non-witness case.
-}
module Data.Env
  ( EnvSchema (..)
  , EnvSchemaW (..)
  , HasDefaultSchema (..)
  , validateEnvWDefault
  , ParseError (..)
  , FieldError (..)
  , renderParseError
  , renderFieldError
  ) where

import Control.Monad.IO.Class
import Data.Env.ExtractFields
import Data.Env.ParseError
import Data.Env.RecordParser
import Data.Env.RecordParserW

-- | Type class for validating plain (non-witness) environment schemas.
class (ExtractFields a, RecordParser a) => EnvSchema a where
  -- | Validate environment variables, mapping field names from camelCase
  -- to UPPER_SNAKE_CASE.
  validateEnv :: MonadIO m => m (Either ParseError a)
  validateEnv = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecord envRaw

  -- | Validate with a custom field-name transform.
  validateEnvWith :: MonadIO m => (String -> String) -> m (Either ParseError a)
  validateEnvWith transform = do
    envRaw <- getEnvRaw @a transform
    return $ parseRecord envRaw

{- | Type class for validating 'Col'-based environment schemas.

The schema value (@a 'Dec@) is passed explicitly to 'validateEnvW', so
parsing behaviour — including defaults — is a runtime choice:

@
validateEnvW mySchema
validateEnvW mySchema { port = ''typeParser' \@Int \`orElse\` 5432 }
@

Use 'validateEnvWDefault' to auto-derive the schema from 'TypeParser'
instances when no overrides are needed.

Field names are converted from camelCase to UPPER_SNAKE_CASE.  Notable
behaviours:

* Consecutive uppercase runs are not split: @myHTTPClient@ → @MY_HTTPCLIENT@.
* A digit before an uppercase letter inserts an underscore: @http2Client@ →
  @HTTP2_CLIENT@.
* A literal underscore passes through: @my_host@ → @MY_HOST@.
-}
class (ExtractFields a, RecordParserW a) => EnvSchemaW a where
  -- | Validate using the supplied schema value.
  validateEnvW :: MonadIO m => a -> m (Either ParseError (RecordParsedType a))
  validateEnvW schema = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecordW schema envRaw

  -- | Validate with a custom field-name transform.
  validateEnvWWith
    :: MonadIO m
    => (String -> String)
    -> a
    -> m (Either ParseError (RecordParsedType a))
  validateEnvWWith transform schema = do
    envRaw <- getEnvRaw @a transform
    return $ parseRecordW schema envRaw

{- | Validate using the auto-derived default schema.

Shorthand for @'validateEnvW' ('defaultSchema' \@a)@; requires every field
type to have a 'TypeParser' instance.
-}
validateEnvWDefault
  :: forall a m
   . (EnvSchemaW (a 'Dec), HasDefaultSchema a, MonadIO m)
  => m (Either ParseError (RecordParsedType (a 'Dec)))
validateEnvWDefault = validateEnvW (defaultSchema @a)
