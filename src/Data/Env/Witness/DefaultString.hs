{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Witness type for parsing string types with type-level default values.
-}
module Data.Env.Witness.DefaultString (
  DefaultString,
) where

import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Proxy
import Data.String
import GHC.TypeLits

-- | Witness type for parsing string types with default values.
--
-- The type parameter @s@ is a type-level symbol that specifies the
-- default value. The type parameter @a@ is the string type to parse.
--
-- This witness allows you to specify a default string value that will be used
-- when the environment variable is missing or empty.
--
-- ==== __Examples__
--
-- >>> parseTypeW (Proxy @(DefaultString "localhost" String)) ""
-- Right "localhost"
--
-- >>> parseTypeW (Proxy @(DefaultString "localhost" String)) "example.com"
-- Right "example.com"
--
data DefaultString (s :: Symbol) a

-- | Parse a string value with a default fallback.
--
-- When the input string is empty, returns the default value specified by the
-- type-level symbol @s@. Otherwise, attempts to parse the string as type @a@.
instance (TypeParser a, IsString a, KnownSymbol s) => TypeParserW (DefaultString s a) a where
  parseTypeW :: Proxy (DefaultString s a) -> String -> Either String a
  parseTypeW _ ""  = Right (fromString $ symbolVal @s Proxy)
  parseTypeW _ str = parseType str
  {-# INLINE parseTypeW #-}
