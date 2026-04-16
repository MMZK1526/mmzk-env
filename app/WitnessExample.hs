{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Env
import Data.Env.RecordParserW
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultNum
import Data.Word
import GHC.Generics

data Config c = Config
  { psqlPort :: Column c (DefaultNum 5432 Word16) Word16  -- Defaults to 5432
  , dbName   :: Column c (Solo String) String }
  deriving (Generic)

instance EnvSchemaW (Config 'Dec)
deriving stock instance Show (Config 'Res)

main :: IO ()
main = do
  -- Demonstrate a validation failure: an invalid port and a missing db name.
  -- Note: psqlPort has a default, so it only fails when a non-empty bad value
  -- is given; dbName has no default so an empty value fails.
  putStrLn "=== Simulated validation failure ==="
  let badEnv = M.fromList [("psqlPort", "not-a-number"), ("dbName", "")]
  case parseRecordW @(Config 'Dec) badEnv of
    Left err -> putStrLn $ renderParseError err
    Right _  -> pure ()

  putStrLn ""

  -- Validate from the actual environment.
  -- Set DB_NAME=mydb (PSQL_PORT is optional, defaults to 5432).
  putStrLn "=== Validating from environment ==="
  errOrConfig <- validateEnvW @(Config 'Dec)
  case errOrConfig of
    Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
    Right cfg -> putStrLn $ "Config with defaults: " ++ show cfg
