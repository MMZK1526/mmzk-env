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
-- The type parameter @a@ is the parsed type (always 'Bool').
--
-- ==== __Examples__
--
-- >>> parseTypeW (Proxy @(DefaultBool 'False Bool)) ""
-- Right False
--
-- >>> parseTypeW (Proxy @(DefaultBool 'True Bool)) ""
-- Right True
--
-- >>> parseTypeW (Proxy @(DefaultBool 'False Bool)) "True"
-- Right True
--
-- >>> parseTypeW' (Proxy @(DefaultBool 'False Bool)) "invalid"
-- Nothing
data DefaultBool (b :: Bool) a

class BoolVal (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance BoolVal 'True where
  boolVal _ = True

instance BoolVal 'False where
  boolVal _ = False

-- | Parse a 'Bool' with a default fallback.
--
-- When the input string is empty, returns the default value specified by the
-- type-level 'Bool' @b@. Otherwise, attempts to parse the string as 'Bool',
-- accepting @True@, @False@, @true@, @false@, @T@, @F@, @t@, @f@, @1@, @0@.
instance BoolVal b => TypeParserW (DefaultBool b Bool) Bool where
  parseTypeW :: Proxy (DefaultBool b Bool) -> String -> Either String Bool
  parseTypeW _ str = case str of
    ""      -> Right (boolVal (Proxy :: Proxy b))
    "true"  -> Right True
    "false" -> Right False
    "T"     -> Right True
    "F"     -> Right False
    "t"     -> Right True
    "f"     -> Right False
    "1"     -> Right True
    "0"     -> Right False
    _       -> parseType str
  {-# INLINE parseTypeW #-}
