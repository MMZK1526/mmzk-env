{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Env.RecordParserW
import Data.Env.Witness.DefaultNum
import qualified Data.Map as M
import Data.Word
import GHC.Generics
import Data.Env.TypeParserW

data Config c = Config
  { psqlPort :: Column c (DefaultNum 5432 Word16) Word16  -- Defaults to 5432
  , dbName   :: Column c (Solo String) String }
  deriving (Generic)

deriving stock instance Show (Config 'Res)  -- For printing the result

-- Parse with defaults
main :: IO ()
main = do
  -- Example 1: Empty environment (uses defaults)
  let emptyEnv = M.empty
  case parseRecordW @(Config 'Dec) emptyEnv of
    Left err  -> putStrLn $ "Parse failed: " ++ err
    Right cfg -> putStrLn $ "Config with defaults: " ++ show cfg

  -- Example 2: Custom values
  let customEnv = M.fromList [("psqlPort", "8080"), ("dbName", "mydb")]
  case parseRecordW @(Config 'Dec) customEnv of
    Left err  -> putStrLn $ "Parse failed: " ++ err
    Right cfg -> putStrLn $ "Config with custom values: " ++ show cfg
