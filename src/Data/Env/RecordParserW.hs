{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : Data.Env.RecordParserW
Description : Schema-driven record parsing with value-level field parsers

Provides 'RecordParserW' for parsing environment-variable schemas whose
fields use the 'Col' column family.

== Design

A @schema 'Dec@ is a plain Haskell record whose fields are functions
@'String' -> 'Either' 'String' a@.  An absent or empty env variable is
passed as @""@; the function decides what to do.  A @schema 'Res@ is the
same record with each field replaced by its resolved value.

@
data DbConfig c = DbConfig
  { host :: Col c Text
  , port :: Col c Int
  }
  deriving (Generic)

instance RecordParserW (DbConfig \'Dec)

-- Default schema (both fields required — no default):
schema :: DbConfig \'Dec
schema = 'defaultSchema' \@DbConfig

-- Schema with runtime defaults, using infix 'orElse':
schema' :: DbConfig \'Dec
schema' = schema
  { host = 'typeParser' \@Text \`orElse\` \"localhost\"
  , port = 'typeParser' \@Int  \`orElse\` 5432
  }
@

== Helpers

* 'typeParser' — builds the standard parser for any 'TypeParser' type.
* 'orElse' — sets the fallback value when the env variable is absent or empty.
* 'fromTypeParserW' — bridges a 'TypeParserW' witness into a field function.
-}
module Data.Env.RecordParserW
  ( RecordParserW (..)
  , HasDefaultSchema (..)
  , ColumnType (..)
  , Col
  , typeParser
  , orElse
  , fromTypeParserW
  ) where

import Data.Env.ParseError
import Data.Env.TypeParser (TypeParser)
import Data.Env.TypeParser qualified as TP
import Data.Env.TypeParserW (TypeParserW)
import Data.Env.TypeParserW qualified as TPW
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics

-- | Column type indicator.
--
-- * 'Dec' — schema column; each field holds a @'String' -> 'Either' 'String' a@ parser.
-- * 'Res' — result column; each field holds the resolved @a@.
data ColumnType = Dec | Res
  deriving stock (Eq, Show)

-- | Type family mapping a column kind to the field representation.
--
-- @'Col' ''Dec' a = 'String' -> 'Either' 'String' a@
-- @'Col' ''Res' a = a@
type family Col (c :: ColumnType) (a :: Type) :: Type where
  Col 'Dec a = String -> Either String a
  Col 'Res a = a

{- | Build the standard field parser for any 'TypeParser' type.

Passes non-empty strings to 'TP.parseType' and treats @""@ (absent or empty
env variable) as a missing value, delegating to 'TP.parseMissing'.  Most
types treat absence as an error (required field); 'Maybe' returns
@'Right' 'Nothing'@.

Combine with 'orElse' to supply a fallback:

@
port = 'typeParser' \@Int \`orElse\` 5432
@
-}
typeParser :: forall a. TypeParser a => String -> Either String a
typeParser "" = TP.parseMissing @a
typeParser s = TP.parseType s
{-# INLINE typeParser #-}

{- | Set the fallback value when the env variable is absent or empty.

Designed for infix use with 'typeParser' or 'fromTypeParserW':

@
port  = 'typeParser' \@Int  \`orElse\` 5432
host  = 'typeParser' \@Text \`orElse\` \"localhost\"
model = 'typeParser' \@Text \`orElse\` \"claude-sonnet-4-6\"
@

@f \`orElse\` d@ returns @'Right' d@ when the variable is absent or empty,
and @f s@ for any non-empty string @s@.
-}
orElse :: (String -> Either String a) -> a -> String -> Either String a
orElse _ d "" = Right d
orElse f _ s = f s
{-# INLINE orElse #-}

{- | Build a field parser from a 'TypeParserW' witness.

Bridges existing witness types into the value-level field API.  Also
composable with 'orElse':

@
fromTypeParserW \@MyCustomWitness \`orElse\` \"fallback\"
@
-}
fromTypeParserW :: forall p a. TypeParserW p a => String -> Either String a
fromTypeParserW "" = TPW.parseMissingW @p Proxy
fromTypeParserW s = TPW.parseTypeW @p Proxy s
{-# INLINE fromTypeParserW #-}

-- | Type class for schemas whose fields are 'Col' columns.
--
-- The method 'parseRecordW' takes the schema value (a @schema 'Dec@) and an
-- env map, returning all field failures collected rather than stopping at
-- the first.
class RecordParserW a where
  -- | The resolved type produced by parsing @a@.
  type RecordParsedType a

  -- | Parse all fields from the env map using the schema's field functions.
  parseRecordW :: a -> Map String String -> Either ParseError (RecordParsedType a)

instance
  ( Generic (a 'Dec)
  , GRecordParserW (Rep (a 'Dec))
  , Generic (a 'Res)
  , GRecordParsedType (Rep (a 'Dec)) () ~ Rep (a 'Res) ()
  ) =>
  RecordParserW (a 'Dec)
  where
  type RecordParsedType (a 'Dec) = a 'Res
  parseRecordW :: a 'Dec -> Map String String -> Either ParseError (a 'Res)
  parseRecordW schema env =
    validationToEither (to <$> gParseRecord (from schema) env)

{- | Generically derive a @schema 'Dec@ whose every field uses 'typeParser'.

Requires every field type to have a 'TypeParser' instance.  Override
individual fields on the returned value to customise parsing:

@
mySchema :: MyConfig \'Dec
mySchema = ('defaultSchema' \@MyConfig)
  { host = 'typeParser' \@Text \`orElse\` \"localhost\" }
@
-}
class HasDefaultSchema a where
  defaultSchema :: a 'Dec

instance (Generic (a 'Dec), GDefaultSchema (Rep (a 'Dec))) => HasDefaultSchema a where
  defaultSchema :: a 'Dec
  defaultSchema = to gDefaultSchema


--------------------------------------------------------------------------------
-- Internal Validation applicative
--------------------------------------------------------------------------------

data Validation e a = VFailure e | VSuccess a

instance Functor (Validation e) where
  fmap _ (VFailure e) = VFailure e
  fmap f (VSuccess a) = VSuccess (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = VSuccess
  VSuccess f <*> VSuccess x = VSuccess (f x)
  VFailure e1 <*> VFailure e2 = VFailure (e1 <> e2)
  VFailure e <*> _ = VFailure e
  _ <*> VFailure e = VFailure e

validationToEither :: Validation e a -> Either e a
validationToEither (VSuccess a) = Right a
validationToEither (VFailure e) = Left e


--------------------------------------------------------------------------------
-- Generic parsing
--------------------------------------------------------------------------------

class GRecordParserW f where
  type GRecordParsedType f :: Type -> Type
  gParseRecord :: f () -> Map String String -> Validation ParseError ((GRecordParsedType f) ())

instance GRecordParserW f => GRecordParserW (M1 D c f) where
  type GRecordParsedType (M1 D c f) = M1 D c (GRecordParsedType f)
  gParseRecord (M1 x) env = M1 <$> gParseRecord x env

instance GRecordParserW f => GRecordParserW (M1 C c f) where
  type GRecordParsedType (M1 C c f) = M1 C c (GRecordParsedType f)
  gParseRecord (M1 x) env = M1 <$> gParseRecord x env

instance (GRecordParserW f, GRecordParserW g) => GRecordParserW (f :*: g) where
  type GRecordParsedType (f :*: g) = GRecordParsedType f :*: GRecordParsedType g
  gParseRecord (x :*: y) env = (:*:) <$> gParseRecord x env <*> gParseRecord y env

instance Selector s => GRecordParserW (M1 S s (K1 i (String -> Either String a))) where
  type GRecordParsedType (M1 S s (K1 i (String -> Either String a))) = M1 S s (K1 i a)
  gParseRecord (M1 (K1 fn)) env =
    let key = selName (undefined :: M1 S s (K1 i a) p)
        result = fn (fromMaybe "" (M.lookup key env))
     in M1 . K1 <$> case result of
          Left msg -> VFailure $ ParseError [FieldError{errField = key, errMessage = msg}]
          Right val -> VSuccess val


--------------------------------------------------------------------------------
-- Generic default schema
--------------------------------------------------------------------------------

class GDefaultSchema f where
  gDefaultSchema :: f ()

instance GDefaultSchema f => GDefaultSchema (M1 D c f) where
  gDefaultSchema :: M1 D c f ()
  gDefaultSchema = M1 gDefaultSchema

instance GDefaultSchema f => GDefaultSchema (M1 C c f) where
  gDefaultSchema :: M1 C c f ()
  gDefaultSchema = M1 gDefaultSchema

instance (GDefaultSchema f, GDefaultSchema g) => GDefaultSchema (f :*: g) where
  gDefaultSchema :: (f :*: g) ()
  gDefaultSchema = gDefaultSchema :*: gDefaultSchema

instance TypeParser a => GDefaultSchema (M1 S s (K1 i (String -> Either String a))) where
  gDefaultSchema :: M1 S s (K1 i (String -> Either String a)) ()
  gDefaultSchema = M1 (K1 (typeParser @a))
