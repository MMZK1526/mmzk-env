# mmzk-env

mmzk-env is a library for reading environment variables into a user-defined data
type. It provides a type-safe way to parse and validate environment variables,
ensuring that they conform to the expected types.

## Quick Start

```Haskell
module Data.Env where

import           Control.Monad.IO.Class
import           Data.Env.ExtractFields
import           Data.Env.RecordParser
import           GHC.Generics

-- | Example: Define an environment schema
data Config = Config
    { port     :: Int
    , name     :: String
    , mainHost :: String
    , debug    :: Maybe Bool }
    deriving (Show, Generic, EnvSchema)

-- | Run the validation
main :: IO ()
main = do
  errOrEnv <- validateEnv @Config
  case errOrEnv of
    Left err  -> putStrLn $ "Validation failed: " ++ err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
```

With this setup, it requires the environment variables `PORT`, `NAME`, `MAIN_HOST`, and `DEBUG` to be set according to the types defined in the `Config` data type. The library will automatically parse these variables and validate them against the schema.

If any variable is missing or has an incorrect type, the validation will fail, and an error message will be printed.

## Enum Support

The library also supports automatic parsing of enumerated types. You can define an enum and derive the `TypeParser` instance using the helper type `EnumParser`.

The extension `DerivingVia` is required for this feature.

```Haskell
{-# LANGUAGE DerivingVia #-}

data Gender = Male | Female
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser Gender)

print $ parseEnum @Gender "Male"   -- Right Male
print $ parseEnum @Gender "Female" -- Right Female
```

## Witness Types: Avoiding Newtype Boilerplate

The library provides a "witness" pattern that allows you to enhance parsing behaviour without wrapping values in newtypes. This is useful when you need features like default values, validation, or transformation but want to keep your final data types simple.

### The Problem: Newtype Boilerplate

Let's say you want to parse a PostgreSQL port that defaults to 5432. Without witnesses, you might create a newtype wrapper:

```Haskell
import Data.Env.TypeParser
import Data.Word

-- Define a newtype wrapper for the port
newtype PsqlPort = PsqlPort Word16
  deriving (Show, Eq)

-- Implement custom parsing with default value
instance TypeParser PsqlPort where
  parseType "" = Right (PsqlPort 5432)  -- Default to 5432
  parseType str = case parseType str of
    Right port -> Right (PsqlPort port)
    Left err   -> Left err

data Config = Config
  { psqlPort :: PsqlPort
  , dbName   :: String }
  deriving (Show, Generic, EnvSchema)
```

Now when you use your config, you have to constantly unwrap the value:

```Haskell
connectToDatabase :: Config -> IO Connection
connectToDatabase cfg = connect $ defaultConnectInfo
  { connectPort = unpackPort (psqlPort cfg)  -- Annoying unpacking!
  , connectDatabase = dbName cfg }
  where
    unpackPort (PsqlPort port) = port
```

### The Solution: Witnesses

With witness types, you can specify parsing behaviour at the type level while keeping the final value unwrapped:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

import Data.Env.RecordParserW
import Data.Env.Witness.DefaultNum
import Data.Word
import GHC.Generics

data Config c = Config
  { psqlPort :: Column c (DefaultNum 5432) Word16  -- Defaults to 5432
  , dbName   :: Column c (Solo String) String }
  deriving (Generic)

deriving stock instance Show (Config 'Res)  -- For printing the result

-- Parse with defaults
main :: IO ()
main = do
  errOrConfig <- parseRecordW @(Config 'Dec) mempty
  case errOrConfig of
    Left err  -> putStrLn $ "Parse failed: " ++ err
    Right cfg -> connectToDatabase cfg  -- cfg :: Config 'Res
```

The magic happens with the `Column` type family and the `ColumnType` phantom type:

- **`Config 'Dec`** (Declaration): The type used for parsing, where each field is `(witness, value)`
  - This works under the hood for the generic instances and users typically don't interact with it directly
- **`Config 'Res`** (Result): The type you work with, where each field is just `value`
- **`Column c witness a`**: Expands to `(witness a, a)` when `c = 'Dec`, or just `a` when `c = 'Res`

Now your final config has no wrappers:

```Haskell
connectToDatabase :: Config 'Res -> IO Connection
connectToDatabase cfg = connect $ defaultConnectInfo
  { connectPort = psqlPort cfg  -- Direct access to Word16!
  , connectDatabase = dbName cfg }
```

### Key Benefits

1. **No Unpacking**: Your final data type contains raw values (Word16, String, etc.), not newtypes
2. **Type-Level Defaults**: Default values are specified in the type signature using type-level naturals
3. **Flexible Parsing**: Different witness types provide different parsing strategies (defaults, validation, transformation)

### Available Witnesses

- **`DefaultNum n a`**: Numeric types with a type-level default value `n`
- **`Solo a`**: Standard parsing without special behaviour (equivalent to `TypeParser`)
- **Custom witnesses**: You can define your own by implementing the `TypeParserW` class

More built-in witnesses will be provided.

For more complex parsing needs, witnesses provide a way to augment behaviour without polluting your domain types with wrapper noise.
