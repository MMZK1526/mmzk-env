{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Env.EnumParser
import Data.Env.TypeParser

data Gender = Male | Female
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser Gender)

main :: IO ()
main = do
  putStrLn "=== Valid values ==="
  print $ parseType @Gender "Male"    -- Right Male
  print $ parseType @Gender "Female"  -- Right Female

  putStrLn ""
  putStrLn "=== Invalid values ==="
  print $ parseType @Gender "male"    -- Left: wrong case
  print $ parseType @Gender "Other"   -- Left: not a constructor
  print $ parseType @Gender ""        -- Left: empty
