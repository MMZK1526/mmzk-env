{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Env
import Data.Env.RecordParser
import GHC.Generics

-- | Example: Define an environment schema
data Config = Config
    { port     :: Int
    , name     :: String
    , mainHost :: String
    , debug    :: Maybe Bool }
    deriving (Show, Generic, EnvSchema)

main :: IO ()
main = do
  -- Demonstrate what a multi-field validation failure looks like.
  -- Two fields are bad: port is not a number, name is empty (missing).
  putStrLn "=== Simulated validation failure ==="
  let badEnv = M.fromList
        [ ("port",     "not-a-number")
        , ("name",     "")             -- empty = missing required field
        , ("mainHost", "localhost") ]
  case parseRecord @Config badEnv of
    Left err -> putStrLn $ renderParseError err
    Right _  -> pure ()

  putStrLn ""

  -- Validate from the actual environment.
  -- Set PORT=8080 NAME=myapp MAIN_HOST=localhost to see the success case.
  putStrLn "=== Validating from environment ==="
  errOrEnv <- validateEnv @Config
  case errOrEnv of
    Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
