{-# LANGUAGE MultiParamTypeClasses #-}

module RecordParserWSpec (spec) where

import Data.Data
import Data.Either ( isLeft )
import Data.Env.RecordParserW
import Data.Env.TypeParser
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultNum
import Data.Map qualified as M
import GHC.Generics
import Test.Hspec

data NameW

instance TypeParserW NameW String where
  parseTypeW :: Proxy NameW -> String -> Either String String
  parseTypeW _ str = case parseType str of
    Right s -> if length s > 16 then Left "Name Too Long!!" else Right s
    left    -> left
  {-# INLINE parseTypeW #-}


data Config c = Config
  { port :: Di (DefaultNum 5432) c Int
  , env  :: Di Solo c String
  , name :: Column c NameW String
  }
  deriving (Generic)

deriving instance Show (Config 'Res)
deriving instance Eq (Config 'Res)

spec :: Spec
spec = describe "parseRecordW for Config" do
  it "uses default port 5432 when port not specified" do
    let envMap = M.fromList [("env", "production"), ("name", "name")]
    parseRecordW @(Config 'Dec) envMap `shouldBe` Right (Config 5432 "production" "name")
  it "parses config with custom port" do
    let envMap = M.fromList [("port", "8080"), ("env", "development"), ("name", "name")]
    parseRecordW @(Config 'Dec) envMap `shouldBe` Right (Config 8080 "development" "name")
  it "fails to parse invalid port value" do
    let envMap = M.fromList [("port", "not-a-number"), ("env", "test"), ("name", "name")]
    parseRecordW @(Config 'Dec) envMap `shouldSatisfy` isLeft
  it "fails to parse invalid name value" do
    let envMap = M.fromList [("env", "test"), ("name", "1234567890poiuytrewq")]
    parseRecordW @(Config 'Dec) envMap `shouldSatisfy` isLeft
