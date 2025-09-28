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
