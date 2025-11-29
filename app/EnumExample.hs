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
  print $ parseType @Gender "Male"    -- Right Male
  print $ parseType @Gender "Female"  -- Right Female
