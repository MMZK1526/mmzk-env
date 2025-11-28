module ConfigParserSpec (spec) where

import Data.Either ( isLeft )
import Data.Env.RecordParserW
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultNum
import Data.Map qualified as M
import GHC.Generics
import Test.Hspec

data Config c = Config
  { port :: Di (DefaultNum 5432) c Int
  , env  :: Di Solo c String
  }
  deriving (Generic)

deriving instance Show (Config 'Res)
deriving instance Eq (Config 'Res)

spec :: Spec
spec = describe "parseRecordW for Config" do
  it "uses default port 5432 when port not specified" do
    let envMap = M.fromList [("env", "production")]
    parseRecordW @(Config 'Dec) envMap `shouldBe` Right (Config 5432 "production")
  it "parses config with custom port" do
    let envMap = M.fromList [("port", "8080"), ("env", "development")]
    parseRecordW @(Config 'Dec) envMap `shouldBe` Right (Config 8080 "development")
  it "fails to parse invalid port value" do
    let envMap = M.fromList [("port", "not-a-number"), ("env", "test")]
    parseRecordW @(Config 'Dec) envMap `shouldSatisfy` isLeft
