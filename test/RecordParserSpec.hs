module RecordParserSpec (spec) where

import Data.Either ( isLeft )
import Data.Env.EnumParser
import Data.Env.RecordParser
import Data.Env.TypeParser
import Data.Map qualified as M
import GHC.Generics
import Test.Hspec

data Gender = Male | Female
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser Gender)

data Person = Person
  { name   :: String
  , age    :: Int
  , gender :: Maybe Gender
  } deriving (Show, Eq, Generic)

spec :: Spec
spec = describe "parseRecord" do
  it "parses a valid record" do
    let env = M.fromList [("name", "Alice"), ("age", "30")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" 30 Nothing)
  it "allows extra fields" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("extra", "value")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" 30 Nothing)
  it "parses optional fields" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("gender", "Female")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" 30 (Just Female))
  it "fails to parse an invalid record" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("gender", "Other")]
    parseRecord @Person env `shouldSatisfy` isLeft
  it "fails to parse a missing field" do
    let env = M.fromList [("name", "Alice")]
    parseRecord @Person env `shouldSatisfy` isLeft
