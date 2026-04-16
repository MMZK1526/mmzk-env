{-# LANGUAGE MultiParamTypeClasses #-}

module RecordParserWSpec (spec) where

import Data.Data
import Data.Env.ParseError
import Data.Env.RecordParserW
import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultNum
import Data.Map qualified as M
import GHC.Generics
import Test.Hspec
import Data.Env.Witness.DefaultBool (DefaultBool)

data NameW

instance TypeParserW NameW String where
  parseTypeW :: Proxy NameW -> String -> Either String String
  parseTypeW _ str = case parseType str of
    Right s -> if length s > 16 then Left "Name Too Long!!" else Right s
    left    -> left
  {-# INLINE parseTypeW #-}


data Config c = Config
  { port  :: Di (DefaultNum 5432) c Int
  , env   :: Di Solo c String
  , name  :: Column c NameW String
  , debug :: Di (DefaultBool 'False) c Bool
  }
  deriving (Generic)

deriving instance Show (Config 'Res)
deriving instance Eq (Config 'Res)

spec :: Spec
spec = describe "parseRecordW for Config" do
  it "uses default port 5432 and default debug False when not specified" do
    let envMap = M.fromList [("env", "production"), ("name", "name")]
    parseRecordW @(Config 'Dec) envMap `shouldBe` Right (Config 5432 "production" "name" False)

  it "parses config with custom port and explicit debug" do
    let envMap = M.fromList [("port", "8080"), ("env", "development"), ("name", "name"), ("debug", "True")]
    parseRecordW @(Config 'Dec) envMap `shouldBe` Right (Config 8080 "development" "name" True)

  it "fails to parse invalid port value" do
    let envMap = M.fromList [("port", "not-a-number"), ("env", "test"), ("name", "name")]
    case parseRecordW @(Config 'Dec) envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "port"
        (head errs).errMessage `shouldSatisfy` (not . null)

  it "fails to parse invalid name value" do
    let envMap = M.fromList [("env", "test"), ("name", "1234567890poiuytrewq")]
    case parseRecordW @(Config 'Dec) envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "name"
        (head errs).errMessage `shouldBe` "Name Too Long!!"

  it "fails to parse invalid debug value" do
    let envMap = M.fromList [("env", "test"), ("name", "name"), ("debug", "yes")]
    case parseRecordW @(Config 'Dec) envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "debug"
        (head errs).errMessage `shouldSatisfy` (not . null)

  it "collects errors from multiple invalid fields in one pass" do
    -- port is invalid; name is too long; env and debug are fine (debug defaults).
    let envMap = M.fromList [("port", "not-a-number"), ("env", "test"), ("name", "1234567890poiuytrewq")]
    case parseRecordW @(Config 'Dec) envMap of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 2
        -- Errors in field-declaration order: port before name.
        map (.errField) errs `shouldBe` ["port", "name"]
        (errs !! 0).errMessage `shouldSatisfy` (not . null)
        (errs !! 1).errMessage `shouldBe` "Name Too Long!!"
