module JsonParserSpec
  ( spec,
  )
where

import Data.Map (fromList)
import JsonParser
import Test.Hspec
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "JSON Parser" $ do
  context "simple cases" $ do
    it "reads null values" $
      parseFromFile jsonValue "example-files/null.json"
        `shouldReturn` Right (JsonObject (fromList [("foo", JsonNull)]))
    it "reads booleans" $
      parseFromFile jsonValue "example-files/booleans.json"
        `shouldReturn` Right (JsonObject (fromList [("true", JsonBool True), ("false", JsonBool False)]))
    it "reads numbers" $
      parseFromFile jsonValue "example-files/numbers.json"
        `shouldReturn` Right
          ( JsonObject
              ( fromList
                  [ ("int", JsonInteger 42),
                    ("neg-sign-int", JsonInteger (-17)),
                    ("float", JsonDouble 123.456),
                    ("neg-sign-float", JsonDouble (-76.54))
                  ]
              )
          )
    it "reads string literals" $
      parseFromFile jsonValue "example-files/strings.json"
        `shouldReturn` Right
          ( JsonObject
              ( fromList
                  [ ("simple", JsonString "Hello, World!"),
                    ("escaped", JsonString "Hello, \"World\"!")
                  ]
              )
          )
    it "reads arrays" $
      parseFromFile jsonValue "example-files/arrays.json"
        `shouldReturn` Right
          ( JsonObject
              ( fromList
                  [ ("empty", JsonArray []),
                    ("mixed", JsonArray [JsonNull, JsonInteger 42, JsonString "foo"]),
                    ("nested", JsonArray [JsonArray [JsonInteger 1], JsonArray [JsonString "bar"]])
                  ]
              )
          )
  context "parse chuck norris jokes" $ do
    let expected = JsonObject (fromList [("type", JsonString "success"), ("value", JsonObject (fromList [("categories", JsonArray []), ("id", JsonInteger 335), ("joke", JsonString "Chuck Norris likes his coffee half and half: half coffee grounds, half wood-grain alcohol.")]))])
    it "should work" $
      parseFromFile jsonValue "example-files/chuck.json" `shouldReturn` Right expected
    it "should work with indented input" $
      parseFromFile jsonValue "example-files/chuck-indented.json" `shouldReturn` Right expected
