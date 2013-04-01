module Main where

import Test.Framework ( Test, defaultMain )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Data.Relational.Types

prop_tuple_degree t = degree t == degree . heading t

test_props = [ ("tmp", prop_foo)
             ]

tests :: [Test]
tests = map testProperty testProps

main :: IO ()
main = defaultMain tests
