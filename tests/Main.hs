module Main where

-- import Control.Monad ( liftM )

import Test.Framework ( Test
--                      , TestName
                      , defaultMain )
--import Test.Framework.Providers.QuickCheck2 ( testProperty )
--import Test.QuickCheck ( Testable, Arbitrary, choose )
--import Test.QuickCheck.Arbitrary ( arbitrary )

--import Data.Relation.Types

--import System.Random ( StdGen, mkStdGen )


-- instance Arbitrary Heading where
--     arbitrary = do len <- choose (1, 512)
--                    -- anames <- take len (repeat arbitrary)
--                    -- tnames <- [arbitrary | _ <- [0..len]]
--                    -- return (fromList $ (zip anames tnames))
--                    xs <- [(arbitrary, arbitrary) | _ <- [0..len]]
--                    liftM fromList xs


-- prop_tuple_degree :: (Degreed a, Headed a, Arbitrary a, Show a, Testable prop) =>
--                       a -> Testable prop
-- prop_tuple_degree :: Tuple -> Bool
--prop_tuple_degree t = tupleDegree == headingDegree
    --where
        --tupleDegree = degree t
        --headingDegree = degree h
        --h = heading t

-----------------------------------------------------------------------------


--names :: Testable a => [(TestName, a)]
--names = [ ("tmp", prop_tuple_degree) ]
--names = []

tests :: [Test]
--tests = map (uncurry testProperty) names

--tests = [((uncurry testProperty) ("tuple degree", prop_tuple_degree))]
tests = []

main :: IO ()
main = defaultMain tests
