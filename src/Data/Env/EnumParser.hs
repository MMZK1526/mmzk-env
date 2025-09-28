{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Data.Env.EnumParser
-- Description: A helper type for parsing Bounded Enums.
--
-- This module provides a 'TypeParser' instance for any type that is an
-- instance of 'Enum', 'Bounded', and 'Show'.
--
-- Example usage:
--
-- > data Gender = Male | Female
-- >   deriving (Show, Eq, Enum, Bounded)
-- >   deriving TypeParser via (EnumParser Gender)
-- >
-- > parseType @Gender "Male" `shouldBe` Right Male
-- > parseType @Gender "Female" `shouldBe` Right Female
-- > parseType @Gender "Other" `shouldSatisfy` isLeft
module Data.Env.EnumParser where

import           Data.Env.TypeParser (TypeParser(..))

-- | A helper type for parsing Bounded Enums.
newtype EnumParser a = EnumParser a
  deriving (Show, Eq)

instance (Enum a, Show a, Bounded a) => TypeParser (EnumParser a) where
  parseType s = case lookup s enumMap of
    Just v  -> Right (EnumParser v)
    Nothing -> Left $ "Cannot parse value: " ++ s ++ ". Valid values are: " ++ show (map fst enumMap)
    where
      enumMap = [(show e, e) | e <- [minBound..maxBound]]
