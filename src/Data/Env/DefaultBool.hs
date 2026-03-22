-- |
-- Module: Data.Env.DefaultBool
-- Description: A helper type for Bool fields with a type-level default value.
--
-- This module provides 'DefaultBool', a newtype wrapping 'Bool' whose
-- 'TypeParser' instance returns a type-level default when the input is empty.
--
-- Example usage:
--
-- > data Config = Config
-- >   { debug   :: DefaultBool 'False
-- >   , verbose :: DefaultBool 'True
-- >   } deriving (Show, Generic, EnvSchema)
-- >
-- > -- DEBUG unset → DefaultBool False
-- > -- DEBUG=True  → DefaultBool True
-- > -- DEBUG=False → DefaultBool False
module Data.Env.DefaultBool where

import Data.Env.TypeParser (TypeParser(..))
import Data.Proxy          (Proxy(..))

-- | A 'Bool' field with a type-level default.
--
-- When the environment variable is absent (empty string) the field takes the
-- value @def@; otherwise the string is parsed as a regular 'Bool'.
newtype DefaultBool (def :: Bool) = DefaultBool Bool
  deriving (Show, Eq, Ord)

-- | Reflect a type-level 'Bool' to a value-level 'Bool'.
class BoolDefault (b :: Bool) where
  boolDefault :: Proxy b -> Bool

instance BoolDefault 'True where
  boolDefault _ = True

instance BoolDefault 'False where
  boolDefault _ = False

instance BoolDefault def => TypeParser (DefaultBool def) where
  parseType "" = Right (DefaultBool (boolDefault (Proxy :: Proxy def)))
  parseType s  = DefaultBool <$> parseType s
  {-# INLINE parseType #-}
