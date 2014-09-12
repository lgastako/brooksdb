-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

--module BrooksSpec where

-- import Test.Framework ( Test
--                       , defaultMain )
-- import Test.Framework.Providers.HUnit ( testCase )
-- import Test.HUnit ( assertEqual )

-- --import Language.Heidi.Lexer ( Var )

-- -- once more I must sally forth without testing.
-- --test_lex_var = assertEqual "var > Var" Var (alexScanTokens "var")

-- tests :: [Test]
-- tests = [
-- --test_lex_var
--         ]

-- main :: IO ()
-- main = defaultMain tests
-- file Spec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
