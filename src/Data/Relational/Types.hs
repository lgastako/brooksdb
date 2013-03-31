module Data.Relational.Types( AttributeName
                            , TypeName
                            , Heading
                            , TupleValue
                            , degree
                            , conforms
                            ) where

--                             , DataType( RelVar
--                                       , MapVar
--                                       , SetVar
--                                       , IntVar
--                                       , FloatVar
--                                       , StringVar
--                                       , BoolVar
--                                       , CharVar
--                                       )
--                             ) where

import qualified Data.Set as Set

-- A heading {H} is a set of ordered pairs or attributes of the form <A,T>, where:

data Heading = Heading AttrSet
    deriving (Show, Eq, Ord)

--     a. A is the name of an attribute of {H}. No two distinct pairs in {H} shall
--        have the same attribute name.

type AttributeName = String

--     b. T is the name of the declared type of attribute A of {H}.

type TypeName = String

-- The number of pairs in {H}—equivalently, the number of attributes of {H}—is the
-- degree of {H}.

hdegree :: Heading -> Int
hdegree = onSet Set.size

--     Now let t be a set of ordered triples <A,T,v>, obtained from {H} by
-- extending each ordered pair <A,T> to include an arbitrary value v of type T,
-- called the attribute value for attribute A of t. Then t is a tuple value
-- (tuple for short) that conforms to heading {H}; equivalently, t is of the
-- corresponding tuple type (see RM Prescription 6). The degree of that heading
-- {H} shall be the degree of t, and the attributes and corresponding types of
-- that heading {H} shall be the attributes and corresponding declared
-- attribute types of t.

data TupleValue = TupleValue AttrValueSet

tdegree :: TupleValue -> Int
tdegree = onVSet Set.size

conforms :: TupleValue -> Heading -> Bool
conforms t h = (degree t) == (degree h) && all (`elem` hs) ts
    where ts = map fst . tupleSet t
          hs = map fst . headingSet h

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

-- NOT YET

-- Level up:

-- We want to be able to grab the attributes from a heading or a tuple or a relation.

class Attributed a where
    attributes :: a -> [AttributeName]

instance Attributed Heading where
    attributes = undefined

instance Attributed TupleValue where
    attributes = undefined

--instance Attributed RelValue where
--    attributes = undefined

-- We want to be able to grab the types from a heading or a tuple or a relation.

class Typed a where
    types :: a -> [TypeName]

instance Typed Heading where
    types = undefined

instance Typed TupleValue where
    types = undefined

--instance Typed RelValue where
--    types = undefined

-- We want to be able to ask the degree of a heading, a tuple or a relation.

class Degreed a where
    degree :: a -> Int

instance Degreed Heading where
    degree = hdegree

instance Degreed TupleValue where
    degree = tdegree

--instance Degreed RelValue where
--    degree = rdegree


-- Helpers for extracting the internal bits of things

withSet :: Heading -> (AttrSet -> a) -> a
--withSet (Heading set) f = f set
withSet = (. headingSet)

headingSet :: Heading -> AttrSet
headingSet (Heading set) = set

tupleSet :: TupleValue -> AttrValueSet
tupleSet (TupleValue set) = set

onSet :: (AttrSet -> a) -> Heading -> a
--onSet f (Heading set) = f set
onSet = (. headingSet)

-- "headingFromList" ... how to naming?
fromList :: [(AttributeName, TypeName)] -> Heading
fromList as = do
    let s = Set.fromList $ map fst as
    if (Set.size s) < length as
        then error "non-unique attribute names"
        else Heading $ Set.fromList as

onVSet :: (AttrValueSet -> a) -> TupleValue -> a
onVSet f (TupleValue set) = f set


-- Misc support for the core stuff

type AttributeValue = String -- For now
type AttrSet = (Set.Set (AttributeName, TypeName))
type AttrValueSet = (Set.Set (AttributeName, TypeName, AttributeValue))

-- eventuallyish
data DataType = RelVar
              | MapVar
              | SetVar
              | IntVar
              | FloatVar
              | StringVar
              | BoolVar
              | CharVar
    deriving (Show, Eq, Ord)

