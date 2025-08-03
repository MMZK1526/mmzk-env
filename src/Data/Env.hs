module Data.Env where

import           Control.Monad.IO.Class
import           Data.Env.ExtractFields
import           Data.Env.RecordParser
import           GHC.Generics

-- | Type class for validating environment schemas.
class (ExtractFields a, RecordParser a) => EnvSchema a where
  validateEnv :: MonadIO m => m (Either String a)
  validateEnv = do
    envRaw <- getEnvRawCamelCaseToUpperSnake @a
    return $ parseRecord envRaw

-- | Example: Define an environment schema
data Config = Config
    { port :: Int
    , name :: String
    , mainHost :: String
    , debug :: Maybe Bool }
    deriving (Show, Generic, EnvSchema)

-- | Run the validation
main :: IO ()
main = do
  errOrEnv <- validateEnv @Config
  case errOrEnv of
    Left err  -> putStrLn $ "Validation failed: " ++ err
    Right cfg -> putStrLn $ "Config loaded successfully: " ++ show cfg
