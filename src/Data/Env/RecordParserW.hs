{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Env.RecordParserW where

import           Data.Map (Map)
import           GHC.Generics
import qualified Data.Map as M
import           Data.Maybe
import Data.Kind
import Data.Env.TypeParserW
import Data.Data

data ColumnType = Dec | Res
  deriving stock (Eq, Show)

type family Column (t :: ColumnType) (p :: Type) (a :: Type) where
  Column 'Dec p a = (p, a)
  Column 'Res p a = a

class RecordParserW a where
  type RecordParsedType a

  parseRecordW :: Map String String -> Either String (RecordParsedType a)

  -- | Parse a record, converting 'Either' to 'Maybe' and dropping any error messages.
  --
  -- This is a convenience function that calls 'parseRecordW' and converts the result
  -- from 'Either String a' to 'Maybe a', discarding the error message on failure.
  parseRecordW' :: Map String String -> Maybe (RecordParsedType a)
  parseRecordW' env = case parseRecordW @a env of
    Right val -> Just val
    Left _    -> Nothing

instance (Generic (a 'Dec), GRecordParserW (Rep (a 'Dec)), Generic (a Res), GRecordParsedType (Rep (a 'Dec)) () ~ Rep (a 'Res) ()) => RecordParserW (a 'Dec) where
  type RecordParsedType (a 'Dec) = a 'Res

  parseRecordW :: Map String String -> Either String (RecordParsedType (a 'Dec))
  parseRecordW a = to <$> gParseRecord @(Rep (a 'Dec)) a


--------------------------------------------------------------------------------
-- Generic instances
--------------------------------------------------------------------------------

-- | Generic validation class.
class GRecordParserW f where
  type GRecordParsedType f :: k -> Type

  gParseRecord :: Map String String -> Either String ((GRecordParsedType f) r)

-- | Handle metadata (wrapping fields in `M1`)
instance GRecordParserW f => GRecordParserW (M1 D c f) where
  type GRecordParsedType (M1 D c f) = M1 D c (GRecordParsedType f)

  gParseRecord :: Map String String -> Either String (GRecordParsedType (M1 D c f) r)
  gParseRecord env = M1 <$> gParseRecord @f env

-- | Handle metadata (wrapping fields in `M1`)
instance GRecordParserW f => GRecordParserW (M1 C c f) where
  type GRecordParsedType (M1 C c f) = M1 C c (GRecordParsedType f)

  gParseRecord :: Map String String -> Either String (GRecordParsedType (M1 C c f) r)
  gParseRecord env = M1 <$> gParseRecord @f env

-- | Handle multiple fields in a record
instance (GRecordParserW f, GRecordParserW g) => GRecordParserW (f :*: g) where
  type GRecordParsedType (f :*: g) = GRecordParsedType f :*: GRecordParsedType g

  gParseRecord :: Map String String -> Either String ((GRecordParsedType f :*: GRecordParsedType g) p)
  gParseRecord env = (:*:) <$> gParseRecord @f env <*> gParseRecord @g env

-- | Handle individual fields
instance (TypeParserW p a, Selector s) => GRecordParserW (M1 S s (K1 i (p, a))) where
  type GRecordParsedType (M1 S s (K1 i (p, a))) = M1 S s (K1 i a)

  gParseRecord :: Map String String -> Either String (M1 S s (K1 i a) r)
  gParseRecord env =
    let key = selName (undefined :: M1 S s (K1 i a) p)
    in  M1 . K1 <$> case parseTypeW @p Proxy (fromMaybe "" $ M.lookup key env) of
        Left err  -> Left $ "Field " ++ show key ++ " parsing error:\n" ++ err
        Right val -> Right val

type Di f c a = Column c (f a) a
