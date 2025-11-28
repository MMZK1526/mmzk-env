{-# LANGUAGE FunctionalDependencies #-}

module Data.Env.TypeParserW (
  TypeParserW (..),
  Solo,
) where

import Data.Env.TypeParser
import Data.Proxy ( Proxy(..) )
import Data.Tuple ( Solo )

-- | Type class for parsers parameterized by a witness type.
--
-- This is similar to 'Data.Env.TypeParser.TypeParser''s 'Data.Env.TypeParser.parseType',
-- but allows a witness object to define how to parse. The witness type @p@ determines
-- the parsing strategy, giving you explicit control over the parsing behavior.
--
-- The witness pattern is useful when:
--
-- * You need multiple different parsing strategies for the same type
-- * [TODO] You want to compose parsers in different ways
--
-- The functional dependency @p -> a@ ensures that each witness type uniquely
-- determines the result type.
class TypeParserW p a | p -> a where
  -- | Parse a value by its string representation using the witness type @p@.
  --
  -- The 'Proxy' parameter carries the witness type information that determines
  -- how the parsing should be performed.
  parseTypeW :: Proxy p -> String -> Either String a

instance TypeParser a => TypeParserW (Solo a) a where
  parseTypeW :: Proxy (Solo a) -> String -> Either String a
  parseTypeW _ = parseType

instance (TypeParserW p1 String, TypeParserW p2 String) => TypeParserW (p1, p2) String where
  parseTypeW :: Proxy (p1, p2) -> String -> Either String String
  parseTypeW _ str = parseTypeW @p1 Proxy str >>= parseTypeW @p2 Proxy

-- newtype DefaultW a = DefaultW a
--   deriving stock (Show, Eq)

-- instance (TypeParser a) => TypeParserW (DefaultW a) a where
--   parseTypeW :: Proxy (DefaultW a) -> String -> Either String a
--   parseTypeW _ = parseType
