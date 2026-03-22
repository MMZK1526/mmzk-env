module DefaultBoolSpec ( spec ) where

import Data.Either ( isLeft )
import Data.Env.DefaultBool
import Data.Env.TypeParser
import Test.Hspec

spec :: Spec
spec = describe "DefaultBool" do
  it "parses True" do
    parseType @(DefaultBool 'False) "True" `shouldBe` Right (DefaultBool True)
    parseType @(DefaultBool 'True)  "True" `shouldBe` Right (DefaultBool True)
  it "parses False" do
    parseType @(DefaultBool 'False) "False" `shouldBe` Right (DefaultBool False)
    parseType @(DefaultBool 'True)  "False" `shouldBe` Right (DefaultBool False)
  it "defaults to False when empty" do
    parseType @(DefaultBool 'False) "" `shouldBe` Right (DefaultBool False)
  it "defaults to True when empty" do
    parseType @(DefaultBool 'True) "" `shouldBe` Right (DefaultBool True)
  it "fails to parse other values" do
    parseType @(DefaultBool 'False) "1"     `shouldSatisfy` isLeft
    parseType @(DefaultBool 'False) "true"  `shouldSatisfy` isLeft
    parseType @(DefaultBool 'False) "false" `shouldSatisfy` isLeft
