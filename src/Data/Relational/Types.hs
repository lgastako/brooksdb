module Data.Relational.Types( AttributeName
                            , TypeName
                            , Heading
                            , Tuple
                            , Relation
                            , degree
                            , conforms
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

-- Most of the guiding documentation in this module is copied from:
--     http://www.dcs.warwick.ac.uk/~hugh/TTM/TTM-2013-02-07.pdf


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

data Tuple = Tuple AttrValueSet

tdegree :: Tuple -> Int
tdegree = onVSet Set.size

tconforms :: Heading -> Tuple -> Bool
tconforms h t = (heading t) == h

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

-- TBD

-- A relation value r (relation for short) consists of a heading and a body,
-- where:

data Relation = Relation Heading -- Body

-- a. The heading of r shall be a heading {H} as defined in RM Prescription 9; r
-- conforms to that heading; equivalently, r is of the corresponding relation type
-- (see RM Prescription 7). The degree of that heading {H} shall be the degree of
-- r, and the attributes and corresponding types of that heading {H} shall be the
-- attributes and corresponding declared attribute types of r.

rdegree :: Relation -> Int
rdegree = degree . rheading

rconforms :: Heading -> Relation -> Bool
rconforms _h _r = undefined

-- b. The body of r shall be a set {b} of tuples, all having that same heading
-- {H}. The cardinality of that body shall be the cardinality of r. Note: Relation
-- r is an empty relation if and only if the set {b} is empty.
--
-- Given a heading {H}, exactly one selector operator S, of declared type RELATION
-- {H}, shall be provided for selecting an arbitrary relation conforming to {H}.
-- That operator S shall have all of the following properties:
--
-- 1. The sole argument to any given invocation of S shall be a set {b} of tuples,
-- each of which shall be denoted by a tuple expression of declared type TUPLE
-- {H}.
--
-- 2. Every relation of type RELATION {H} shall be produced by some invocation of
-- S for which the tuple expressions that together denote the argument to that
-- invocation are all literals.The Third Manifesto 7
--
-- 3. Every successful invocation of S shall produce some relation of type
-- RELATION {H}: to be specific, the relation of type RELATION {H} with body {b}.





-- Level up:


class Headed a where
    heading :: a -> Heading


instance Headed Tuple where
    heading = theading

instance Headed Relation where
    heading = rheading

-- We want to be able to grab the attributes from a heading or a tuple or a relation.

class Attributed a where
    attributes :: a -> [AttributeName]

instance Attributed Heading where
    attributes = undefined

instance Attributed Tuple where
    attributes = undefined

instance Attributed Relation where
    attributes = undefined

-- We want to be able to grab the types from a heading or a tuple or a relation.

class Typed a where
    types :: a -> [TypeName]

instance Typed Heading where
    types = undefined

instance Typed Tuple where
    types = undefined

instance Typed Relation where
    types = undefined

-- We want to be able to ask the degree of a heading, a tuple or a relation.

class Degreed a where
    degree :: a -> Int

instance Degreed Heading where
    degree = hdegree

instance Degreed Tuple where
    degree = tdegree

instance Degreed Relation where
    degree = rdegree

-- We want to be able to determine if a Tuple or a Relation conforms to a heading.

class Conformant a where
    conforms :: Heading -> a -> Bool

instance Conformant Tuple where
    conforms = tconforms

instance Conformant Relation where
    conforms = rconforms



-- Helpers for extracting the internal bits of things

headingSet :: Heading -> AttrSet
headingSet (Heading set) = set

--tupleSet :: Tuple -> AttrValueSet
--tupleSet (Tuple set) = set

rheading :: Relation -> Heading
rheading (Relation h) = h

theading :: Tuple -> Heading
theading = undefined

onSet :: (AttrSet -> a) -> Heading -> a
--onSet f (Heading set) = f set
onSet = (. headingSet)

-- "headingFromList" ... how to naming?
--fromList :: [(AttributeName, TypeName)] -> Heading
--fromList as = do
--    let s = Set.fromList $ map fst as
--    if (Set.size s) < length as
--        then error "non-unique attribute names"
--        else Heading $ Set.fromList as

onVSet :: (AttrValueSet -> a) -> Tuple -> a
onVSet f (Tuple set) = f set


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
