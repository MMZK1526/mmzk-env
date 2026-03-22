module EnumParserSpec ( spec ) where

import Data.Either ( isLeft )
import Data.Env.EnumParser
import Data.Env.TypeParser
import Test.Hspec

-- Test enum types
data Gender = Male | Female
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser Gender)

data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser LogLevel)

spec :: Spec
spec = describe "EnumParser" do
  describe "parse Gender enum" do
    it "parses Male" do
      parseType @Gender "Male" `shouldBe` Right Male
    it "parses Female" do
      parseType @Gender "Female" `shouldBe` Right Female
    it "fails to parse invalid value" do
      parseType @Gender "Other" `shouldSatisfy` isLeft
      parseType @Gender "male" `shouldSatisfy` isLeft
      parseType @Gender "MALE" `shouldSatisfy` isLeft
      parseType @Gender "" `shouldSatisfy` isLeft

  describe "parse LogLevel enum" do
    it "parses Debug" do
      parseType @LogLevel "Debug" `shouldBe` Right Debug
    it "parses Info" do
      parseType @LogLevel "Info" `shouldBe` Right Info
    it "parses Warning" do
      parseType @LogLevel "Warning" `shouldBe` Right Warning
    it "parses Error" do
      parseType @LogLevel "Error" `shouldBe` Right Error
    it "fails to parse invalid values" do
      parseType @LogLevel "debug" `shouldSatisfy` isLeft
      parseType @LogLevel "INFO" `shouldSatisfy` isLeft
      parseType @LogLevel "Warn" `shouldSatisfy` isLeft
      parseType @LogLevel "" `shouldSatisfy` isLeft
