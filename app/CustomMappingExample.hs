{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (toUpper)
import Data.Env
import GHC.Generics

-- | Example: Define an environment schema with custom mapping
data Config = Config
    { port      :: Int
    , name      :: String
    , main_host :: String
    , debug     :: Maybe Bool }
    deriving (Show, Generic, EnvSchema)

-- | Run the validation with custom mapping
-- This will look for environment variables: PORT, NAME, MAINHOST, DEBUG
-- Note: main_host maps to MAINHOST (not MAIN_HOST) with map toUpper
main :: IO ()
main = do
  errOrEnv <- validateEnvWith @Config (map toUpper)
  case errOrEnv of
    Left err  -> putStrLn $ "Validation failed: " ++ err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
