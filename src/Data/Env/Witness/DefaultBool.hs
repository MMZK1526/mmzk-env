{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Witness type for parsing 'Bool' with a type-level default value.
-}
module Data.Env.Witness.DefaultBool (
  DefaultBool,
) where

import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Proxy

-- | Witness type for parsing 'Bool' with a default value.
--
-- The type parameter @b@ is a type-level 'Bool' that specifies the default
-- value used when the environment variable is absent (empty string).
--
-- ==== __Examples__
--
-- >>> parseTypeW (Proxy @(DefaultBool 'False)) ""
-- Right False
--
-- >>> parseTypeW (Proxy @(DefaultBool 'True)) ""
-- Right True
--
-- >>> parseTypeW (Proxy @(DefaultBool 'False)) "True"
-- Right True
--
-- >>> parseTypeW' (Proxy @(DefaultBool 'False)) "invalid"
-- Nothing
data DefaultBool (b :: Bool)

class BoolVal (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance BoolVal 'True where
  boolVal _ = True

instance BoolVal 'False where
  boolVal _ = False

-- | Parse a 'Bool' with a default fallback.
--
-- When the input string is empty, returns the default value specified by the
-- type-level 'Bool' @b@. Otherwise, attempts to parse the string as 'Bool'.
instance BoolVal b => TypeParserW (DefaultBool b) Bool where
  parseTypeW :: Proxy (DefaultBool b) -> String -> Either String Bool
  parseTypeW _ ""  = Right (boolVal (Proxy :: Proxy b))
  parseTypeW _ str = parseType str
  {-# INLINE parseTypeW #-}
