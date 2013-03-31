module Main where

import Test.Framework ( Test, defaultMain )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

tmpTest :: String -> Bool
tmpTest x = x == x

tests :: [Test]
tests = [ (testProperty "tmp" tmpTest)
        ]

main :: IO ()
main = defaultMain tests
