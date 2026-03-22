module DefaultBoolSpec ( spec ) where

import Data.Either ( isLeft )
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultBool
import Data.Proxy
import Test.Hspec

spec :: Spec
spec = describe "DefaultBool" do
  it "defaults to False when empty" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "" `shouldBe` Right False
  it "defaults to True when empty" do
    parseTypeW (Proxy @(DefaultBool 'True Bool)) "" `shouldBe` Right True
  it "parses True / False" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "True"  `shouldBe` Right True
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "False" `shouldBe` Right False
  it "parses true / false" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "true"  `shouldBe` Right True
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "false" `shouldBe` Right False
  it "parses T / F" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "T" `shouldBe` Right True
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "F" `shouldBe` Right False
  it "parses t / f" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "t" `shouldBe` Right True
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "f" `shouldBe` Right False
  it "parses 1 / 0" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "1" `shouldBe` Right True
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "0" `shouldBe` Right False
  it "fails to parse other values" do
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "yes"     `shouldSatisfy` isLeft
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "no"      `shouldSatisfy` isLeft
    parseTypeW (Proxy @(DefaultBool 'False Bool)) "invalid" `shouldSatisfy` isLeft
