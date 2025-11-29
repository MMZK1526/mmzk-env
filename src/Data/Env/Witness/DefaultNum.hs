{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Witness type for parsing numeric types with type-level default values.
-}
module Data.Env.Witness.DefaultNum (
  DefaultNum,
) where

import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Proxy
import GHC.TypeLits

-- | Witness type for parsing numeric types with default values.
--
-- The type parameter @n@ is a type-level natural number that specifies the
-- default value. The type parameter @a@ is the numeric type to parse.
--
-- This witness allows you to specify a default numeric value that will be used
-- when the environment variable is missing or empty.
--
-- ==== __Examples__
--
-- >>> parseTypeW (Proxy @(DefaultNum 5432 Int)) ""
-- Right 5432
--
-- >>> parseTypeW (Proxy @(DefaultNum 5432 Int)) "8080"
-- Right 8080
--
-- >>> parseTypeW' (Proxy @(DefaultNum 5432 Int)) "invalid"
-- Nothing
data DefaultNum (n :: Nat) a

-- | Parse a numeric value with a default fallback.
--
-- When the input string is empty, returns the default value specified by the
-- type-level natural @n@. Otherwise, attempts to parse the string as type @a@.
instance (TypeParser a, Num a, KnownNat n) => TypeParserW (DefaultNum n a) a where
  parseTypeW :: Proxy (DefaultNum n a) -> String -> Either String a
  parseTypeW _ ""  = Right (fromInteger $ natVal @n Proxy)
  parseTypeW _ str = parseType str
  {-# INLINE parseTypeW #-}
