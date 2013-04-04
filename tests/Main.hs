module Main where

import Test.Framework ( Test
                      , defaultMain )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( assertEqual )

import Language.Heidi.Lexer ( Var )

test_lex_var = assertEqual "var > Var" Var (alexScanTokens "var")

tests :: [Test]
tests = [ test_lex_var
        ]

main :: IO ()
main = defaultMain tests
