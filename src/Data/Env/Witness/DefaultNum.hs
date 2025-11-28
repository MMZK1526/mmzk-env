{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Env.Witness.DefaultNum (
  DefaultNum (..),
) where

import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Proxy
import GHC.TypeLits

-- | Witness type for parsing numeric types with default values.
--
-- This witness allows you to specify a default numeric value that will be used
-- when the environment variable is missing or empty.
newtype DefaultNum (n :: Nat) a = DefaultNum a
  deriving stock (Show, Eq)

instance (TypeParser a, Num a, KnownNat n) => TypeParserW (DefaultNum n a) a where
  parseTypeW :: Proxy (DefaultNum n a) -> String -> Either String a
  parseTypeW _ ""  = Right (fromInteger $ natVal @n Proxy)
  parseTypeW _ str = parseType str
  {-# INLINE parseTypeW #-}
