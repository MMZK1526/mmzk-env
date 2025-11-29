{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Env
import Data.Env.Witness.DefaultNum
import Data.Word
import GHC.Generics
import Data.Env.RecordParserW
import Data.Env.TypeParserW

data Config c = Config
  { psqlPort :: Column c (DefaultNum 5432 Word16) Word16  -- Defaults to 5432
  , dbName   :: Column c (Solo String) String }
  deriving (Generic)

instance EnvSchemaW (Config 'Dec)
deriving stock instance Show (Config 'Res)  -- For printing the result

-- Validate environment variables with defaults
main :: IO ()
main = do
  errOrConfig <- validateEnvW @(Config 'Dec)
  case errOrConfig of
    Left err  -> putStrLn $ "Validation failed: " ++ err
    Right cfg -> putStrLn $ "Config with defaults: " ++ show cfg
