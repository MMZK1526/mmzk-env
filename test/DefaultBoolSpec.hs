module DefaultBoolSpec ( spec ) where

import Data.Either ( isLeft )
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultBool
import Data.Proxy
import Test.Hspec

spec :: Spec
spec = describe "DefaultBool" do
  it "parses True" do
    parseTypeW (Proxy @(DefaultBool 'False)) "True" `shouldBe` Right True
    parseTypeW (Proxy @(DefaultBool 'True))  "True" `shouldBe` Right True
  it "parses False" do
    parseTypeW (Proxy @(DefaultBool 'False)) "False" `shouldBe` Right False
    parseTypeW (Proxy @(DefaultBool 'True))  "False" `shouldBe` Right False
  it "defaults to False when empty" do
    parseTypeW (Proxy @(DefaultBool 'False)) "" `shouldBe` Right False
  it "defaults to True when empty" do
    parseTypeW (Proxy @(DefaultBool 'True)) "" `shouldBe` Right True
  it "fails to parse other values" do
    parseTypeW (Proxy @(DefaultBool 'False)) "1"     `shouldSatisfy` isLeft
    parseTypeW (Proxy @(DefaultBool 'False)) "true"  `shouldSatisfy` isLeft
    parseTypeW (Proxy @(DefaultBool 'False)) "false" `shouldSatisfy` isLeft
