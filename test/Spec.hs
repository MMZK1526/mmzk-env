import           Data.Either (isLeft)
import           Data.Env.RecordParser
import           Data.Env.TypeParser
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map as M
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Generics
import           Test.Hspec

main :: IO ()
main = do
  hspec typeParserSpecSpec
  hspec recordParserSpec

typeParserSpecSpec :: Spec
typeParserSpecSpec = describe "parseType" do
  describe "parse String" do
    it "parses non-empty string" do
      parseType @String "hello" `shouldBe` Right "hello"
    it "does not parse empty string" do
      parseType @String "" `shouldSatisfy` isLeft
  describe "parse Bool" do
    it "parses True" do
      parseType @Bool "True" `shouldBe` Right True
    it "parses False" do
      parseType @Bool "False" `shouldBe` Right False
    it "fails to parse other values" do
      parseType @Bool "1" `shouldSatisfy` isLeft
      parseType @Bool "true" `shouldSatisfy` isLeft
      parseType @Bool "false" `shouldSatisfy` isLeft
  describe "parse ()" do
    it "parses ()" do
      parseType @() "()" `shouldBe` Right ()
    it "fails to parse other values" do
      parseType @() "" `shouldSatisfy` isLeft
      parseType @() "1" `shouldSatisfy` isLeft
  describe "parse Int8" do
    it "parses 0" do
      parseType @Int8 "0" `shouldBe` Right 0
    it "parses -128" do
      parseType @Int8 "-128" `shouldBe` Right (-128)
    it "parses 127" do
      parseType @Int8 "127" `shouldBe` Right 127
    it "fails to parse other values" do
      parseType @Int8 "128" `shouldSatisfy` isLeft
      parseType @Int8 "-129" `shouldSatisfy` isLeft
      parseType @Int8 "1.0" `shouldSatisfy` isLeft
  describe "parse Int16" do
    it "parses 0" do
      parseType @Int16 "0" `shouldBe` Right 0
    it "parses -32768" do
      parseType @Int16 "-32768" `shouldBe` Right (-32768)
    it "parses 32767" do
      parseType @Int16 "32767" `shouldBe` Right 32767
    it "fails to parse other values" do
      parseType @Int16 "32768" `shouldSatisfy` isLeft
      parseType @Int16 "-32769" `shouldSatisfy` isLeft
      parseType @Int16 "1.0" `shouldSatisfy` isLeft
  describe "parse Int32" do
    it "parses 0" do
      parseType @Int32 "0" `shouldBe` Right 0
    it "parses -2147483648" do
      parseType @Int32 "-2147483648" `shouldBe` Right (-2147483648)
    it "parses 2147483647" do
      parseType @Int32 "2147483647" `shouldBe` Right 2147483647
    it "fails to parse other values" do
      parseType @Int32 "2147483648" `shouldSatisfy` isLeft
      parseType @Int32 "-2147483649" `shouldSatisfy` isLeft
      parseType @Int32 "1.0" `shouldSatisfy` isLeft
  describe "parse Int64" do
    it "parses 0" do
      parseType @Int64 "0" `shouldBe` Right 0
    it "parses -9223372036854775808" do
      parseType @Int64 "-9223372036854775808" `shouldBe` Right (-9223372036854775808)
    it "parses 9223372036854775807" do
      parseType @Int64 "9223372036854775807" `shouldBe` Right 9223372036854775807
    it "fails to parse other values" do
      parseType @Int64 "9223372036854775808" `shouldSatisfy` isLeft
      parseType @Int64 "-9223372036854775809" `shouldSatisfy` isLeft
      parseType @Int64 "1.0" `shouldSatisfy` isLeft
  describe "parse Int" do
    it "parses 0" do
      parseType @Int "0" `shouldBe` Right 0
    it "parses min Int" do
      parseType @Int (show (minBound :: Int)) `shouldBe` Right (minBound :: Int)
    it "parses max Int" do
      parseType @Int (show (maxBound :: Int)) `shouldBe` Right (maxBound :: Int)
    it "fails to parse other values" do
      parseType @Int (show ((fromIntegral (maxBound :: Int) :: Integer) + 1)) `shouldSatisfy` isLeft
      parseType @Int (show ((fromIntegral (minBound :: Int) :: Integer) - 1)) `shouldSatisfy` isLeft
      parseType @Int "1.0" `shouldSatisfy` isLeft
  describe "parse Word8" do
    it "parses 0" do
      parseType @Word8 "0" `shouldBe` Right 0
    it "parses 255" do
      parseType @Word8 "255" `shouldBe` Right 255
    it "fails to parse other values" do
      parseType @Word8 "256" `shouldSatisfy` isLeft
      parseType @Word8 "-1" `shouldSatisfy` isLeft
      parseType @Word8 "1.0" `shouldSatisfy` isLeft
  describe "parse Word16" do
    it "parses 0" do
      parseType @Word16 "0" `shouldBe` Right 0
    it "parses 65535" do
      parseType @Word16 "65535" `shouldBe` Right 65535
    it "fails to parse other values" do
      parseType @Word16 "65536" `shouldSatisfy` isLeft
      parseType @Word16 "-1" `shouldSatisfy` isLeft
      parseType @Word16 "1.0" `shouldSatisfy` isLeft
  describe "parse Word32" do
    it "parses 0" do
      parseType @Word32 "0" `shouldBe` Right 0
    it "parses 4294967295" do
      parseType @Word32 "4294967295" `shouldBe` Right 4294967295
    it "fails to parse other values" do
      parseType @Word32 "4294967296" `shouldSatisfy` isLeft
      parseType @Word32 "-1" `shouldSatisfy` isLeft
      parseType @Word32 "1.0" `shouldSatisfy` isLeft
  describe "parse Word64" do
    it "parses 0" do
      parseType @Word64 "0" `shouldBe` Right 0
    it "parses 18446744073709551615" do
      parseType @Word64 "18446744073709551615" `shouldBe` Right 18446744073709551615
    it "fails to parse other values" do
      parseType @Word64 "18446744073709551616" `shouldSatisfy` isLeft
      parseType @Word64 "-1" `shouldSatisfy` isLeft
      parseType @Word64 "1.0" `shouldSatisfy` isLeft
  describe "parse Word" do
    it "parses 0" do
      parseType @Word "0" `shouldBe` Right 0
    it "parses max Word" do
      parseType @Word (show (maxBound :: Word)) `shouldBe` Right (maxBound :: Word)
    it "fails to parse other values" do
      parseType @Word (show ((fromIntegral (maxBound :: Word) :: Integer) + 1)) `shouldSatisfy` isLeft
      parseType @Word "-1" `shouldSatisfy` isLeft
      parseType @Word "1.0" `shouldSatisfy` isLeft
  describe "parse Maybe" do
    it "should success" do
      parseType @(Maybe Int) "1" `shouldBe` Right (Just 1)
      parseType @(Maybe Int) "" `shouldBe` Right Nothing
      parseType @(Maybe String) "hello" `shouldBe` Right (Just "hello")
      parseType @(Maybe String) "" `shouldBe` Right Nothing
    it "should fail" do
      parseType @(Maybe Int) "hello" `shouldSatisfy` isLeft
      parseType @(Maybe Bool) "fALSE" `shouldSatisfy` isLeft

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

recordParserSpec :: Spec
recordParserSpec = describe "parseRecord" do
  it "parses a valid record" do
    let env = M.fromList [("name", "Alice"), ("age", "30")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" 30)
  it "allows extra fields" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("extra", "value")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" 30)
  it "fails to parse an invalid record" do
    let env = M.fromList [("name", "Alice"), ("age", "thirty")]
    parseRecord @Person env `shouldSatisfy` isLeft
  it "fails to parse a missing field" do
    let env = M.fromList [("name", "Alice")]
    parseRecord @Person env `shouldSatisfy` isLeft
