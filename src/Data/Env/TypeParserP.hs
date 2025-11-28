{-# LANGUAGE FunctionalDependencies #-}

module Data.Env.TypeParserP (
  TypeParserP (..),
) where

import Data.Env.TypeParser ( TypeParser(..) )
import Data.Proxy ( Proxy(..) )
import Data.Tuple ( Solo )

class TypeParserP p a | p -> a where
  -- | parse a value by its string representation.
  parseTypeP :: Proxy p -> String -> Either String a

instance TypeParser a => TypeParserP (Solo a) a where
  parseTypeP :: Proxy (Solo a) -> String -> Either String a
  parseTypeP _ = parseType

instance (TypeParserP p1 String, TypeParserP p2 String) => TypeParserP (p1, p2) String where
  parseTypeP :: Proxy (p1, p2) -> String -> Either String String
  parseTypeP _ str = parseTypeP @p1 Proxy str >>= parseTypeP @p2 Proxy

newtype Default a = Default a
  deriving stock (Show, Eq)

instance TypeParser a => TypeParserP (Default a) a where
  parseTypeP :: Proxy (Default a) -> String -> Either String a
  parseTypeP _ = parseType
