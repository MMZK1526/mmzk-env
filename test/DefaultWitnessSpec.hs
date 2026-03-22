module DefaultWitnessSpec ( spec ) where

import Data.Either ( isLeft )
import Data.Env.TypeParserW
import Data.Env.Witness.DefaultNum
import Data.Env.Witness.DefaultString
import Data.Int ( Int16, Int32, Int64 )
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word ( Word16, Word32, Word64 )
import Test.Hspec

spec :: Spec
spec = do
  describe "DefaultNum witness" do
    describe "with Int" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 5432 Int)) "" `shouldBe` Right 5432
      it "parses non-empty valid value" do
        parseTypeW (Proxy @(DefaultNum 5432 Int)) "8080" `shouldBe` Right 8080
      it "parses negative value" do
        parseTypeW (Proxy @(DefaultNum 5432 Int)) "-100" `shouldBe` Right (-100)
      it "fails to parse invalid value" do
        parseTypeW (Proxy @(DefaultNum 5432 Int)) "invalid" `shouldSatisfy` isLeft
        parseTypeW (Proxy @(DefaultNum 5432 Int)) "1.5" `shouldSatisfy` isLeft

    describe "with Word16" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 3000 Word16)) "" `shouldBe` Right 3000
      it "parses non-empty valid value" do
        parseTypeW (Proxy @(DefaultNum 3000 Word16)) "8080" `shouldBe` Right 8080
      it "fails to parse out-of-range value" do
        parseTypeW (Proxy @(DefaultNum 3000 Word16)) "70000" `shouldSatisfy` isLeft
        parseTypeW (Proxy @(DefaultNum 3000 Word16)) "-1" `shouldSatisfy` isLeft

    describe "with Int16" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 100 Int16)) "" `shouldBe` Right 100
      it "parses non-empty valid value" do
        parseTypeW (Proxy @(DefaultNum 100 Int16)) "32000" `shouldBe` Right 32000
      it "fails to parse out-of-range value" do
        parseTypeW (Proxy @(DefaultNum 100 Int16)) "40000" `shouldSatisfy` isLeft

    describe "with Int32" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 1000 Int32)) "" `shouldBe` Right 1000
      it "parses large value" do
        parseTypeW (Proxy @(DefaultNum 1000 Int32)) "2000000000" `shouldBe` Right 2000000000

    describe "with Int64" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 999 Int64)) "" `shouldBe` Right 999
      it "parses very large value" do
        parseTypeW (Proxy @(DefaultNum 999 Int64)) "9000000000000" `shouldBe` Right 9000000000000

    describe "with Word32" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 500 Word32)) "" `shouldBe` Right 500
      it "parses large value" do
        parseTypeW (Proxy @(DefaultNum 500 Word32)) "4000000000" `shouldBe` Right 4000000000

    describe "with Word64" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultNum 777 Word64)) "" `shouldBe` Right 777
      it "parses very large value" do
        parseTypeW (Proxy @(DefaultNum 777 Word64)) "18000000000000000000" `shouldBe` Right 18000000000000000000

  describe "DefaultString witness" do
    describe "with String" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultString "localhost" String)) "" `shouldBe` Right "localhost"
      it "parses non-empty valid value" do
        parseTypeW (Proxy @(DefaultString "localhost" String)) "example.com" `shouldBe` Right "example.com"
      it "handles special characters" do
        parseTypeW (Proxy @(DefaultString "default" String)) "hello world!" `shouldBe` Right "hello world!"

    describe "with strict Text" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultString "default-host" T.Text)) "" `shouldBe` Right "default-host"
      it "parses non-empty valid value" do
        parseTypeW (Proxy @(DefaultString "default-host" T.Text)) "production.com" `shouldBe` Right "production.com"

    describe "with lazy Text" do
      it "returns default value for empty string" do
        parseTypeW (Proxy @(DefaultString "lazy-default" TL.Text)) "" `shouldBe` Right "lazy-default"
      it "parses non-empty valid value" do
        parseTypeW (Proxy @(DefaultString "lazy-default" TL.Text)) "actual-value" `shouldBe` Right "actual-value"

    describe "different default values" do
      it "handles empty default string" do
        parseTypeW (Proxy @(DefaultString "" String)) "" `shouldBe` Right ""
      it "handles URL as default" do
        parseTypeW (Proxy @(DefaultString "http://localhost:8080" String)) "" `shouldBe` Right "http://localhost:8080"
      it "handles path as default" do
        parseTypeW (Proxy @(DefaultString "/var/log/app.log" String)) "" `shouldBe` Right "/var/log/app.log"
