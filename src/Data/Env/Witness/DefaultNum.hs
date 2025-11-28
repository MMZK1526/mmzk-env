{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Data.Env.Witness.DefaultNum
Description : Witness type for parsing numeric types with default values
Copyright   : (c) 2025
License     : MIT
Maintainer  : maintainer@example.com
Stability   : experimental

This module provides the 'DefaultNum' witness type, which allows you to parse
numeric environment variables with a type-level default value. When the environment
variable is missing or empty, the default value is used instead.

== Usage Example

@
import Data.Env.RecordParserW
import Data.Env.Witness.DefaultNum
import GHC.Generics

data Config c = Config
  { port :: Di (DefaultNum 5432) c Int
  , timeout :: Di (DefaultNum 30) c Int
  }
  deriving (Generic)

-- When parsing with an empty map, defaults are used:
-- parseRecordW \@(Config 'Dec) mempty
-- => Right (Config 5432 30)

-- When values are provided, they override the defaults:
-- parseRecordW \@(Config 'Dec) (fromList [("port", "8080")])
-- => Right (Config 8080 30)
@
-}
module Data.Env.Witness.DefaultNum (
  DefaultNum (..),
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
newtype DefaultNum (n :: Nat) a = DefaultNum a
  deriving stock (Show, Eq)

-- | Parse a numeric value with a default fallback.
--
-- When the input string is empty, returns the default value specified by the
-- type-level natural @n@. Otherwise, attempts to parse the string as type @a@.
instance (TypeParser a, Num a, KnownNat n) => TypeParserW (DefaultNum n a) a where
  parseTypeW :: Proxy (DefaultNum n a) -> String -> Either String a
  parseTypeW _ ""  = Right (fromInteger $ natVal @n Proxy)
  parseTypeW _ str = parseType str
  {-# INLINE parseTypeW #-}
