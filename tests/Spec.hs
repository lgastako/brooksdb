import Test.Hspec
import Test.QuickCheck
import Control.Exception ( evaluate )

import Data.Relation.Types ( fromList
                           , asKeyDefList
                           )


main :: IO ()
main = hspec $ do
  describe "asKeyDefList" $ do
    context "empty set"  $ do
      let heading0 = fromList []
      it "returns an empty string" $ do
        (asKeyDefList heading0) `shouldBe` ""

    context "one item" $ do
      let heading1 = fromList [ ("foo", "String") ]
      it "returns a single keyDef" $ do
        (asKeyDefList heading1) `shouldBe` "\n    foo String\n"

    context "two items" $ do
      let heading2 = fromList [ ("foo", "String")
                              , ("bar", "Int")
                              ]
      it "returns a single keyDef" $ do
        (asKeyDefList heading2) `shouldBe` (  "\n"
                                           ++ "    bar Int,\n"
                                           ++ "    foo String\n"
                                           )

    context "three items" $ do
      let heading3 = fromList [ ("foo", "String")
                              , ("bar", "Int")
                              , ("baz", "Bool")
                              ]
      it "returns a single keyDef" $ do
        (asKeyDefList heading3) `shouldBe` (  "\n"
                                           ++ "    bar Int,\n"
                                           ++ "    baz Bool,\n"
                                           ++ "    foo String\n"
                                           )
