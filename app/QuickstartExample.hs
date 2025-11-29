{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Env
import GHC.Generics

-- | Example: Define an environment schema
data Config = Config
    { port     :: Int
    , name     :: String
    , mainHost :: String
    , debug    :: Maybe Bool }
    deriving (Show, Generic, EnvSchema)

-- | Run the validation
main :: IO ()
main = do
  errOrEnv <- validateEnv @Config
  case errOrEnv of
    Left err  -> putStrLn $ "Validation failed: " ++ err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
