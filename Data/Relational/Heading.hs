module Data.Relational.Heading( Heading
                              , fromList
                              , degree
                              ) where

import qualified Data.Set as Set

-- A heading {H} is a set of ordered pairs or attributes of the form <A,T>, where:
--
--     a. A is the name of an attribute of {H}. No two distinct pairs in {H} shall
--        have the same attribute name.
--
--     b. T is the name of the declared type of attribute A of {H}.
--
-- The number of pairs in {H}—equivalently, the number of attributes of {H}—is the
-- degree of {H}.

type AttributeName = String
type TypeName = String

type AttrSet = (Set.Set (AttributeName, TypeName))

data Heading = Heading AttrSet
    deriving (Show, Eq, Ord)

--withSet :: Heading -> (AttrSet -> a) -> a
--withSet (Heading set) f = f set

onSet :: (AttrSet -> a) -> Heading -> a
onSet f (Heading set) = f set

degree :: Heading -> Int
degree = onSet Set.size

--fromList = Heading . Set.fromList
--it was so pretty!

fromList :: [(AttributeName, AttributeValue)] -> Heading
fromList as = do
    let s = Set.fromList $ map fst as
    if (Set.size s) < length as
        then error "non-unique attribute names"
        else Heading $ Set.fromList as
