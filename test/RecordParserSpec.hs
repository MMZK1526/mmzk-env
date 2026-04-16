module RecordParserSpec ( spec ) where

import Data.Coerce ( coerce )
import Data.Env.EnumParser
import Data.Env.ParseError
import Data.Env.RecordParser
import Data.Env.TypeParser
import Data.Map qualified as M
import GHC.Generics
import Test.Hspec

data Gender = Male | Female
  deriving (Show, Eq, Enum, Bounded)
  deriving TypeParser via (EnumParser Gender)

newtype Age = Age Int
  deriving (Show, Eq)

instance TypeParser Age where
  parseType :: String -> Either String Age
  parseType str = case parseType @Int str of
    Right a -> if a >= 0 && a <= 120 then Right $ Age a else Left "Age is not within range!"
    left    -> coerce left

data Person = Person
  { name   :: String
  , age    :: Age
  , gender :: Maybe Gender
  } deriving (Show, Eq, Generic)

spec :: Spec
spec = describe "parseRecord" do
  it "parses a valid record" do
    let env = M.fromList [("name", "Alice"), ("age", "30")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" (Age 30) Nothing)

  it "allows extra fields" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("extra", "value")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" (Age 30) Nothing)

  it "parses optional fields" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("gender", "Female")]
    parseRecord @Person env `shouldBe` Right (Person "Alice" (Age 30) (Just Female))

  it "fails to parse an invalid enum value" do
    let env = M.fromList [("name", "Alice"), ("age", "30"), ("gender", "Other")]
    case parseRecord @Person env of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "gender"
        (head errs).errMessage `shouldContain` "\"Other\""
        (head errs).errMessage `shouldContain` "Male"
        (head errs).errMessage `shouldContain` "Female"

  it "fails to parse an invalid age" do
    let env = M.fromList [("name", "Alice"), ("age", "150"), ("gender", "Female")]
    case parseRecord @Person env of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "age"
        (head errs).errMessage `shouldBe` "Age is not within range!"

  it "fails to parse a missing required field" do
    let env = M.fromList [("name", "Alice")]
    case parseRecord @Person env of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        length errs `shouldBe` 1
        (head errs).errField `shouldBe` "age"
        (head errs).errMessage `shouldBe` "missing required environment variable"

  it "collects errors from multiple invalid fields in one pass" do
    let env = M.fromList [("name", "Alice"), ("age", "150"), ("gender", "Other")]
    case parseRecord @Person env of
      Right _ -> expectationFailure "expected Left"
      Left (ParseError errs) -> do
        -- Both age and gender fail; name succeeds.
        length errs `shouldBe` 2
        -- Errors are in field-declaration order.
        map (.errField) errs `shouldBe` ["age", "gender"]
        (errs !! 0).errMessage `shouldBe` "Age is not within range!"
        (errs !! 1).errMessage `shouldContain` "\"Other\""
