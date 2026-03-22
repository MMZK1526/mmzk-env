{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Env
import Data.Env.TypeParser
import Data.Word
import GHC.Generics

-- Define a newtype wrapper for the port
newtype PsqlPort = PsqlPort Word16
  deriving (Show, Eq)

-- Implement custom parsing with default value
instance TypeParser PsqlPort where
  parseType "" = Right (PsqlPort 5432)  -- Default to 5432
  parseType str = case parseType str of
    Right port -> Right (PsqlPort port)
    Left err   -> Left err

data Config = Config
  { psqlPort :: PsqlPort
  , dbName   :: String }
  deriving (Show, Generic, EnvSchema)

-- Now when you use your config, you have to constantly unwrap the value:
unpackPort :: PsqlPort -> Word16
unpackPort (PsqlPort port) = port

main :: IO ()
main = do
  errOrConfig <- validateEnv @Config
  case errOrConfig of
    Left err  -> putStrLn $ "Parse failed: " ++ err
    Right cfg -> putStrLn $ "Port: " ++ show (unpackPort $ psqlPort cfg)
