{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Data.Env.TypeParser
-- Description: Type class that provides parsers for types.
--
-- This module provides a type class 'TypeParser' that provides parsers for
-- different types. The parsers are used to parse environment variables from
-- their string representation.
module Data.Env.TypeParser (
  TypeParser (..),
) where

import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Tuple ( Solo(..) )
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Generics
import qualified Text.Gigaparsec as P
import qualified Text.Gigaparsec.Char as P
import qualified Text.Gigaparsec.Combinator as P
import qualified Text.Gigaparsec.Errors.ErrorGen as P
import qualified Text.Gigaparsec.Errors.Combinator as P
import qualified Text.Gigaparsec.Token.Descriptions as L
import qualified Text.Gigaparsec.Token.Lexer as L

-- | Type class for parsers associated with types.
class TypeParser a where
  -- | parse a value by its string representation.
  parseType :: String -> Either String a

  default parseType
    :: (Generic a, GTypeParser (Rep a)) => String -> Either String a
  parseType s = to <$> gTypeParser s
  {-# INLINE parseType #-}

-- | Required (non-empty) String field.
--
-- in POSIX systems, an empty env variable is equivalent to an undefined env
-- variable. To ensure consistency across platforms, we require that all
-- environment variables are non-empty.
instance TypeParser String where
  parseType :: String -> Either String String
  parseType = parse (P.some P.item)
  {-# INLINE parseType #-}

-- | Required @Integer@ field (parsed from String).
instance TypeParser Integer where
  parseType :: String -> Either String Integer
  parseType = parse (L.decimal integerParser)
  {-# INLINE parseType #-}

-- | Required @Int@ field (parsed from String).
instance TypeParser Int where
  parseType :: String -> Either String Int
  parseType = (fromInteger <$>) . parse do
      P.filterSWith (simpleErrorGen "Int out of bound")
                    validateInt
                    (L.decimal integerParser)
    where
      validateInt n = n >= fromIntegral @Int minBound
                   && n <= fromIntegral @Int maxBound
  {-# INLINE parseType #-}

-- | Required @Word@ field (parsed from String).
instance TypeParser Word where
  parseType :: String -> Either String Word
  parseType = (fromInteger <$>) . parse do
      P.filterSWith (simpleErrorGen "Word out of bound")
                    validateWord
                    (L.decimal naturalParser)
    where
      validateWord n = n <= fromIntegral @Word maxBound
  {-# INLINE parseType #-}

-- | Required @Bool@ field (parsed from String).
instance TypeParser Bool where
  parseType :: String -> Either String Bool
  parseType = parse do
    P.choice [P.string "True" P.$> True, P.string "False" P.$> False]
  {-# INLINE parseType #-}

-- | Required @Int8@ field (parsed from String).
instance TypeParser Int8 where
  parseType :: String -> Either String Int8
  parseType = parse (L.decimal8 integerParser)
  {-# INLINE parseType #-}

-- | Required @Int16@ field (parsed from String).
instance TypeParser Int16 where
  parseType :: String -> Either String Int16
  parseType = parse (L.decimal16 integerParser)
  {-# INLINE parseType #-}

-- | Required @Int32@ field (parsed from String).
instance TypeParser Int32 where
  parseType :: String -> Either String Int32
  parseType = parse (L.decimal32 integerParser)
  {-# INLINE parseType #-}

-- | Required @Int64@ field (parsed from String).
instance TypeParser Int64 where
  parseType :: String -> Either String Int64
  parseType = parse (L.decimal64 integerParser)
  {-# INLINE parseType #-}

-- | Required @Word8@ field (parsed from String).
instance TypeParser Word8 where
  parseType :: String -> Either String Word8
  parseType = parse (L.decimal8 naturalParser)
  {-# INLINE parseType #-}

-- | Required @Word16@ field (parsed from String).
instance TypeParser Word16 where
  parseType :: String -> Either String Word16
  parseType = parse (L.decimal16 naturalParser)
  {-# INLINE parseType #-}

-- | Required @Word32@ field (parsed from String).
instance TypeParser Word32 where
  parseType :: String -> Either String Word32
  parseType = parse (L.decimal32 naturalParser)
  {-# INLINE parseType #-}

-- | Required @Word64@ field (parsed from String).
instance TypeParser Word64 where
  parseType :: String -> Either String Word64
  parseType = parse (L.decimal64 naturalParser)
  {-# INLINE parseType #-}

-- | Required strict @Text@ field (parsed from String)
instance TypeParser T.Text where
  parseType :: String -> Either String T.Text
  parseType = fmap T.pack . parseType
  {-# INLINE parseType #-}

-- | Required lazy @Text@ field (parsed from String)
instance TypeParser TL.Text where
  parseType :: String -> Either String TL.Text
  parseType = fmap TL.pack . parseType
  {-# INLINE parseType #-}

-- | Required @()@ field (parsed from String).
instance TypeParser () where
  parseType :: String -> Either String ()
  parseType = parse (P.string "()" P.$> ())
  {-# INLINE parseType #-}

-- | Solo fields isomorphic to the original (@Solo a@).
instance TypeParser a => TypeParser (Solo a) where
  parseType :: String -> Either String (Solo a)
  parseType s = MkSolo <$> parseType s
  {-# INLINE parseType #-}

-- | Optional fields (@Maybe a@).
instance TypeParser a => TypeParser (Maybe a) where
  parseType :: String -> Either String (Maybe a)
  parseType "" = Right Nothing
  parseType s  = Just <$> parseType s
  {-# INLINE parseType #-}


--------------------------------------------------------------------------------
-- Generic instances
--------------------------------------------------------------------------------

-- | Generic validation class.
class GTypeParser f where
  gTypeParser :: String -> Either String (f p)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

simpleLexeme :: L.Lexeme
simpleLexeme = L.nonlexeme (L.mkLexer L.plain)
{-# INLINE simpleLexeme #-}

integerParser :: L.IntegerParsers L.CanHoldSigned
integerParser = L.integer simpleLexeme
{-# INLINE integerParser #-}

naturalParser :: L.IntegerParsers L.CanHoldUnsigned
naturalParser = L.natural simpleLexeme
{-# INLINE naturalParser #-}

parseResultToEither :: P.Result String a -> Either String a
parseResultToEither (P.Failure e) = Left (show e)
parseResultToEither (P.Success a) = Right a
{-# INLINE parseResultToEither #-}

parse :: P.Parsec a -> String -> Either String a
parse parser = parseResultToEither . P.parse (parser >>= (P.eof P.$>))
{-# INLINE parse #-}

simpleErrorGen :: String -> P.ErrorGen a
simpleErrorGen msg = case P.vanillaGen of
  P.VanillaGen {..} -> P.VanillaGen { reason = const (Just msg), .. }
  impossible        -> impossible
{-# INLINE simpleErrorGen #-}
