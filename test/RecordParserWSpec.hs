{-# LANGUAGE MultiParamTypeClasses #-}

module RecordParserWSpec (spec) where

import Data.Env.ParseError
import Data.Env.RecordParserW
import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultBool (DefaultBool)
import Data.Map qualified as M
import Data.Proxy (Proxy (..))
import GHC.Generics
import Test.Hspec

-- Custom field parser: names may not exceed 16 characters.
data NameW

instance TypeParserW NameW String where
  parseTypeW :: Proxy NameW -> String -> Either String String
  parseTypeW _ str = case parseType str of
    Right s -> if length s > 16 then Left "Name Too Long!!" else Right s
    left -> left

data Config c = Config
  { port  :: Col c Int
  , env   :: Col c String
  , name  :: Col c String
  , debug :: Col c Bool
  }
  deriving stock (Generic)

deriving stock instance Show (Config 'Res)
deriving stock instance Eq (Config 'Res)

-- Default schema: port defaults to 5432, debug to False via witness;
-- name uses the NameW witness; env is required.
defaultConfig :: Config 'Dec
defaultConfig = Config
  { port  = typeParser @Int    `orElse` 5432
  , env   = typeParser @String
  , name  = fromTypeParserW @NameW
  , debug = fromTypeParserW @(DefaultBool 'False Bool)
  }

spec :: Spec
spec = describe "parseRecordW for Config" do
  it "uses default port 5432 and default debug False when not specified" do
    let envMap = M.fromList [("env", "production"), ("name", "name")]
    parseRecordW defaultConfig envMap `shouldBe` Right (Config 5432 "production" "name" False)

  it "parses config with custom port and explicit debug" do
    let envMap = M.fromList [("port", "8080"), ("env", "development"), ("name", "name"), ("debug", "True")]
    parseRecordW defaultConfig envMap `shouldBe` Right (Config 8080 "development" "name" True)

  it "fails to parse invalid port value" do
    let envMap = M.fromList [("port", "not-a-number"), ("env", "test"), ("name", "name")]
    case parseRecordW defaultConfig envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "port"
        (head errs).errMessage `shouldSatisfy` (not . null)

  it "fails to parse invalid name value" do
    let envMap = M.fromList [("env", "test"), ("name", "1234567890poiuytrewq")]
    case parseRecordW defaultConfig envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "name"
        (head errs).errMessage `shouldBe` "Name Too Long!!"

  it "fails to parse invalid debug value" do
    let envMap = M.fromList [("env", "test"), ("name", "name"), ("debug", "yes")]
    case parseRecordW defaultConfig envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "debug"
        (head errs).errMessage `shouldSatisfy` (not . null)

  it "collects errors from multiple invalid fields in one pass" do
    let envMap = M.fromList [("port", "not-a-number"), ("env", "test"), ("name", "1234567890poiuytrewq")]
    case parseRecordW defaultConfig envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 2
        map (.errField) errs `shouldBe` ["port", "name"]
        (errs !! 0).errMessage `shouldSatisfy` (not . null)
        (errs !! 1).errMessage `shouldBe` "Name Too Long!!"

  it "orElse overrides the fallback at runtime" do
    let altConfig :: Config 'Dec
        altConfig = defaultConfig{port = typeParser @Int `orElse` 9999}
        envMap = M.fromList [("env", "test"), ("name", "name")]
        expected :: Config 'Res
        expected = Config 9999 "test" "name" False
    parseRecordW altConfig envMap `shouldBe` Right expected
