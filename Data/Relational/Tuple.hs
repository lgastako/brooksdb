module Data.Relational.Tuple( Heading
                            , fromList
                            , hdegree
                            , TupleValue
                            , tdegree
                            , DataType( RelVar
                                      , MapVar
                                      , SetVar
                                      , IntVar
                                      , FloatVar
                                      , StringVar
                                      , BoolVar
                                      , CharVar
                                      )
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
--
--     Now let t be a set of ordered triples <A,T,v>, obtained from {H} by
-- extending each ordered pair <A,T> to include an arbitrary value v of type T,
-- called the attribute value for attribute A of t. Then t is a tuple value (tuple
-- for short) that conforms to heading {H}; equivalently, t is of the
-- corresponding tuple type (see RM Prescription 6). The degree of that heading
-- {H} shall be the degree of t, and the attributes and corresponding types of
-- that heading {H} shall be the attributes and corresponding declared attribute
-- types of t.
--
--     Given a heading {H}, exactly one selector operator S, of declared type
-- TUPLE {H}, shall be provided for selecting an arbitrary tuple conforming to
-- {H}. That operator S shall have all of the following properties:
--
-- 1. There shall be a one to one correspondence between the parameters of S and
-- the attributes of {H}.  Each parameter of S shall have the same declared type
-- as the corresponding attribute of {H}.
--
-- 2. Every tuple of type TUPLE {H} shall be produced by some invocation of S in
-- which every argument expression is a literal.
--
-- 3. Every successful invocation of S shall produce some tuple of type TUPLE {H}.

-- NOTES:
-- This definition is so simple that we could almost literally go with a Data.Set
-- of ordered pairs (tuples) but in the interest of later switching to a better
-- implementation rather than just representing them as they are literally
-- defined, we will hide the implementation.

type AttributeName = String
type TypeName = String

type AttributeValue = String -- For now

type AttrSet = (Set.Set (AttributeName, TypeName))
type AttrValueSet = (Set.Set (AttributeName, TypeName, AttributeValue))

data Heading = Heading AttrSet
    deriving (Show, Eq, Ord)

--withSet :: Heading -> (AttrSet -> a) -> a
--withSet (Heading set) f = f set

onSet :: (AttrSet -> a) -> Heading -> a
onSet f (Heading set) = f set

hdegree :: Heading -> Int
hdegree = onSet Set.size

--mkHeading = Heading . Set.fromList
--it was so pretty!

fromList :: [(AttributeName, AttributeValue)] -> Heading
fromList as = do
    let s = Set.fromList $ map fst as
    if (Set.size s) < length as
        then error "non-unique attribute names"
        else Heading $ Set.fromList as

data TupleValue = TupleValue AttrValueSet

onVSet :: (AttrValueSet -> a) -> TupleValue -> a
onVSet f (TupleValue set) = f set

tdegree :: TupleValue -> Int
tdegree = onVSet Set.size

data DataType = RelVar
              | MapVar
              | SetVar
              | IntVar
              | FloatVar
              | StringVar
              | BoolVar
              | CharVar
    deriving (Show, Eq, Ord)

