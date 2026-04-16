{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Env
import Data.Env.RecordParser
import Data.Env.TypeParser
import Data.Word
import GHC.Generics

-- | A newtype wrapper for a PostgreSQL port.
newtype PsqlPort = PsqlPort Word16
  deriving (Show, Eq)

-- | Custom parser: default to 5432 when the variable is absent.
instance TypeParser PsqlPort where
  parseMissing = Right (PsqlPort 5432)
  parseType str = case parseType str of
    Right port -> Right (PsqlPort port)
    Left err   -> Left err

data Config = Config
  { psqlPort :: PsqlPort
  , dbName   :: String }
  deriving (Show, Generic, EnvSchema)

unpackPort :: PsqlPort -> Word16
unpackPort (PsqlPort p) = p

main :: IO ()
main = do
  -- Demonstrate a validation failure: an out-of-range port and a missing name.
  putStrLn "=== Simulated validation failure ==="
  let badEnv = M.fromList [("psqlPort", "99999"), ("dbName", "")]
  case parseRecord @Config badEnv of
    Left err -> putStrLn $ renderParseError err
    Right _  -> pure ()

  putStrLn ""

  -- Validate from the actual environment.
  -- Set PSQL_PORT=5432 DB_NAME=mydb to see the success case.
  putStrLn "=== Validating from environment ==="
  errOrConfig <- validateEnv @Config
  case errOrConfig of
    Left err  -> putStrLn $ "Parse failed:\n" ++ renderParseError err
    Right cfg -> putStrLn $ "Port: " ++ show (unpackPort $ psqlPort cfg)
