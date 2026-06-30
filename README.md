# mmzk-env

mmzk-env is a library for reading environment variables into a user-defined data
type. It provides a type-safe way to parse and validate environment variables,
ensuring that they conform to the expected types.

## Contents

- [mmzk-env](#mmzk-env)
  - [Contents](#contents)
  - [Quick Start](#quick-start)
    - [Custom Environment Variable Mapping](#custom-environment-variable-mapping)
  - [Error Handling](#error-handling)
  - [Enum Support](#enum-support)
  - [Witness Types: Avoiding Newtype Boilerplate](#witness-types-avoiding-newtype-boilerplate)
    - [The Problem: Newtype Boilerplate](#the-problem-newtype-boilerplate)
    - [The Solution: Witnesses](#the-solution-witnesses)
    - [Key Benefits](#key-benefits)
    - [Available Witnesses](#available-witnesses)

## Quick Start

**[Full example →][quickstart-example]**

```Haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Data.Env
import GHC.Generics

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
    Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
```

With this setup, it requires the environment variables `PORT`, `NAME`, `MAIN_HOST`, and `DEBUG` to be set according to the types defined in the `Config` data type. The library will automatically parse these variables and validate them against the schema.

If any variable is missing or has an incorrect type, the validation will fail, and an error message will be printed.

### Custom Environment Variable Mapping

**[Full example →][custom-mapping-example]**

By default, the library converts camelCase field names to UPPER_SNAKE_CASE (e.g., `mainHost` → `MAIN_HOST`).

If you want to use uppercase environment variable names without underscores (like `MAINHOST` instead of `MAIN_HOST`), you can use `validateEnvWith` (or `validateEnvWWith` for witness types) with a custom mapping function:

```Haskell
data Config = Config
    { port      :: Int
    , name      :: String
    , main_host :: String  -- Will map to "MAINHOST" with custom mapping
    , debug     :: Maybe Bool }
    deriving (Show, Generic, EnvSchema)

main :: IO ()
main = do
  errOrEnv <- validateEnvWith @Config (map toUpper)
  case errOrEnv of
    Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
```

With `validateEnvWith (map toUpper)`, the field `main_host` will look for the environment variable `MAINHOST` instead of `MAIN_HOST`.

You can provide any custom mapping function to `validateEnvWith` to transform field names to environment variable names according to your needs.

## Error Handling

`validateEnv` (and its variants) returns `Either ParseError a`. Unlike a simple `Either String` approach, `ParseError` collects **all** field failures in a single pass — so every invalid field is reported at once, not just the first one.

Use `renderParseError` to format the error for display:

```Haskell
case errOrEnv of
  Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
  Right cfg -> ...
```

A single-field failure shows the field name, then the detail indented below:

```
port: invalid field
  (line 1, column 1):
    unexpected "n"
    expected an integer
    >not-a-number
     ^
```

A missing required field gives a clear message instead of a parser error:

```
name: invalid field
  missing required environment variable
```

Multiple failures are numbered in field-declaration order:

```
2 fields failed to parse:
  1. port: invalid field
     (line 1, column 1):
       unexpected "n"
       expected an integer
       >not-a-number
        ^
  2. name: invalid field
     missing required environment variable
```

`ParseError` and `FieldError` are both exported from `Data.Env`, so you can also inspect them programmatically:

```Haskell
import Data.Env (ParseError(..), FieldError(..))

case errOrEnv of
  Right cfg -> ...
  Left (ParseError errs) ->
    mapM_ (\fe -> putStrLn $ fe.errField ++ " is invalid") errs
```

## Enum Support

**[Full example →][enum-example]**

The library also supports automatic parsing of enumerated types. You can define an enum and derive the `TypeParser` instance using the helper type `EnumParser`.

The extension `DerivingVia` is required for this feature.

```Haskell
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

data Gender = Male | Female
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser Gender)

parseType @Gender "Male"    -- Right Male
parseType @Gender "Female"  -- Right Female
parseType @Gender "male"    -- Left "invalid value \"male\"; expected one of: Male, Female"
```

Enum parsing is case-sensitive and the error message lists all valid constructors.

## Schema Types: Value-Level Parsing with Defaults

The library provides a schema pattern that keeps your final data type clean (no newtype wrappers, no unpacking) while allowing defaults, validation, and custom parsing to be specified as plain Haskell values.

### The Problem: Newtype Boilerplate

**[Full example →][newtype-example]**

Suppose you want a PostgreSQL port that defaults to 5432. Without the schema pattern you might write a newtype:

```Haskell
newtype PsqlPort = PsqlPort Word16 deriving (Show, Eq)

instance TypeParser PsqlPort where
  parseMissing = Right (PsqlPort 5432)
  parseType str = PsqlPort <$> parseType str

data Config = Config { psqlPort :: PsqlPort, dbName :: String }
  deriving (Show, Generic, EnvSchema)
```

Now every use site must unwrap `PsqlPort`:

```Haskell
connectToDatabase cfg = connect defaultConnectInfo
  { connectPort = unpackPort (psqlPort cfg)  -- annoying unpacking
  , connectDatabase = dbName cfg }
```

### The Solution: Schema Records

**[Full example →][witness-example]**

Define the config with the `Col` column family. Each field in `'Dec` holds a parser function; each field in `'Res` holds the resolved value:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Env
import Data.Env.RecordParserW
import Data.Word
import GHC.Generics

data Config c = Config
  { psqlPort :: Col c Word16
  , dbName   :: Col c String }
  deriving (Generic)

instance EnvSchemaW (Config 'Dec)
deriving stock instance Show (Config 'Res)

-- Schema value: psqlPort defaults to 5432, dbName is required.
defaultConfig :: Config 'Dec
defaultConfig = Config
  { psqlPort = typeParser @Word16 `orElse` 5432
  , dbName   = typeParser @String }

main :: IO ()
main = do
  errOrConfig <- validateEnvW defaultConfig
  case errOrConfig of
    Left err  -> putStrLn $ "Validation failed:\n" ++ renderParseError err
    Right cfg -> connectToDatabase cfg  -- cfg :: Config 'Res
```

The two column kinds:

- **`Config 'Dec`** — a real runtime value. Each field is a `String -> Either String a` parser function. Pass it to `validateEnvW`; swap individual fields for different defaults or custom logic.
- **`Config 'Res`** — the result after parsing. Each field is just the plain value.
- **`Col c a`** — expands to `String -> Either String a` when `c = 'Dec`, or `a` when `c = 'Res`.

No wrappers in the result:

```Haskell
connectToDatabase :: Config 'Res -> IO Connection
connectToDatabase cfg = connect defaultConnectInfo
  { connectPort    = cfg.psqlPort  -- direct Word16, no unwrapping
  , connectDatabase = cfg.dbName }
```

### Runtime overrides

Because the schema is a value, you can override any field before parsing:

```Haskell
-- Different default for a different environment:
stagingConfig :: Config 'Dec
stagingConfig = defaultConfig { psqlPort = typeParser @Word16 `orElse` 5433 }

-- Custom validation:
strictDbConfig :: Config 'Dec
strictDbConfig = defaultConfig
  { dbName = \s -> do
      name <- typeParser @String s
      if length name > 32 then Left "DB name too long" else Right name }
```

### Auto-derived schemas

If every field type has a `TypeParser` instance, `defaultSchema` derives the whole `'Dec` value automatically (all fields required, no defaults):

```Haskell
autoConfig :: Config 'Dec
autoConfig = defaultSchema @Config

-- Or validate directly:
result <- validateEnvWDefault @Config
```

### Helpers

- **`typeParser @T`** — standard parser for `T` from its `TypeParser` instance. Required by default (absent variable → error); `Maybe T` fields return `Right Nothing` when absent.
- **`f \`orElse\` d`** — wrap parser `f` so an absent or empty variable returns `Right d`.
- **`fromTypeParserW @W`** — build a field parser from a `TypeParserW` witness `W` (see below).

### Witness bridge

Existing witnesses implement `TypeParserW` and remain usable via `fromTypeParserW`:

- **`DefaultNum n a`** — numeric type with type-level default `n`.
- **`DefaultString s a`** — string type with type-level default `s`.
- **`DefaultBool b Bool`** — lenient bool parser (accepts `true`/`1`/`t`) with type-level default `b`.
- **Custom witnesses** — implement `TypeParserW` and pass to `fromTypeParserW`.

```Haskell
-- Equivalent ways to default psqlPort to 5432:
psqlPort = typeParser @Word16 `orElse` 5432
psqlPort = fromTypeParserW @(DefaultNum 5432 Word16)
```

[quickstart-example]: https://github.com/MMZK1526/mmzk-env/blob/main/app/QuickstartExample.hs
[custom-mapping-example]: https://github.com/MMZK1526/mmzk-env/blob/main/app/CustomMappingExample.hs
[enum-example]: https://github.com/MMZK1526/mmzk-env/blob/main/app/EnumExample.hs
[newtype-example]: https://github.com/MMZK1526/mmzk-env/blob/main/app/NewtypeExample.hs
[witness-example]: https://github.com/MMZK1526/mmzk-env/blob/main/app/WitnessExample.hs
