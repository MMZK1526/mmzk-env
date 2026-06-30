{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Env
import Data.Env.RecordParserW
import Data.Env.Witness.DefaultNum (DefaultNum)
import qualified Data.Map.Strict as M
import Data.Word (Word16)
import GHC.Generics

data Config c = Config
  { psqlPort :: Col c Word16
  , dbName   :: Col c String
  }
  deriving stock (Generic)

instance EnvSchemaW (Config 'Dec)

deriving stock instance Show (Config 'Res)

-- Default schema: psqlPort defaults to 5432 via witness; dbName is required.
defaultConfig :: Config 'Dec
defaultConfig = Config
  { psqlPort = fromTypeParserW @(DefaultNum 5432 Word16)
  , dbName   = typeParser @String
  }

main :: IO ()
main = do
  putStrLn "=== Simulated validation failure ==="
  let badEnv = M.fromList [("psqlPort", "not-a-number"), ("dbName", "")]
  case parseRecordW defaultConfig badEnv of
    Left err -> putStrLn $ renderParseError err
    Right _  -> pure ()

  putStrLn ""

  putStrLn "=== Validating from environment ==="
  errOrConfig <- validateEnvW defaultConfig
  case errOrConfig of
    Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
    Right cfg -> putStrLn $ "Config with defaults: " ++ show cfg
