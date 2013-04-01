module Data.Relation.Types( AttributeName
                            , TypeName
                            , Heading
                            , fromList
                            , Tuple
                            , headWith
                            , withNoHead
                            , Relation
                            , withHeading
                            , insertTuple
                            , Headed
                            , Degreed
                            , degree
                            , conforms
                            , attributes
                            , heading
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

import Data.String.Utils ( join )
import Data.List ( sort )

-- Most of the guiding documentation in this module is copied from:
--     http://www.dcs.warwick.ac.uk/~hugh/TTM/TTM-2013-02-07.pdf


-- A heading {H} is a set of ordered pairs or attributes of the form <A,T>, where:

data Heading = Heading AttrSet
    deriving (Eq, Ord, Show)


fromList :: [(AttributeName, TypeName)] -> Heading
fromList = Heading . Set.fromList


--     a. A is the name of an attribute of {H}. No two distinct pairs in {H} shall
--        have the same attribute name.

type AttributeName = String

--     b. T is the name of the declared type of attribute A of {H}.

type TypeName = String

-- The number of pairs in {H}—equivalently, the number of attributes of {H}—is the
-- degree of {H}.

-- extending each ordered pair <A,T> to include an arbitrary value v of type T,
-- called the attribute value for attribute A of t. Then t is a tuple value
-- (tuple for short) that conforms to heading {H}; equivalently, t is of the
-- corresponding tuple type (see RM Prescription 6). The degree of that heading
-- {H} shall be the degree of t, and the attributes and corresponding types of
-- that heading {H} shall be the attributes and corresponding declared
-- attribute types of t.

data Tuple = Tuple Heading AttrValueSet
    deriving (Eq, Ord)

data AnonTuple = AnonTuple AttrValueSet
    deriving (Eq)

instance Show Tuple where
    show (Tuple (Heading h) vset) =
        "TUPLE {" ++ attrList ++ "}"
            where attrList = if (Set.size vset) > 0
                                 then "\n" ++ (join ",\n" prettyAttrs) ++ "\n"
                                 else ""
                  prettyAttrs = map paint atvs
                  paint (a, t, v) = "    " ++ a ++ "::" ++ t ++ "=\"" ++ v ++ "\""
                  atvs = zip3 as ts vs
                  as = map fst hlist
                  ts = map snd hlist
                  vs = map snd vlist
                  vlist = sort $ Set.toList vset
                  hlist = sort $ Set.toList h

withNoHead :: [(AttributeName, AttributeValue)] -> AnonTuple
withNoHead as = AnonTuple (Set.fromList as)

headWith :: Heading -> [(AttributeName, AttributeValue)] -> Tuple
headWith hd as
    | (length as) /= (degree hd) = error "degree mismatch."
    | anames /= hnames           = error "attribute name mismatch."
    | otherwise = Tuple hd tset
        -- TODO: Also check that the types are correct.  for the lolz.
        where tset = (Set.fromList (zip anames values))
              anames = map fst sas
              values = map snd sas
              hnames = map fst shs
              hset = headSet hd
              headSet (Heading set) = set
              sas = sort as
              shs = sort $ Set.toList hset

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

data Relation = Relation Heading (Set.Set Tuple)
    deriving (Eq)

withHeading :: Heading -> Relation
withHeading hd = Relation hd Set.empty


-- a. The heading of r shall be a heading {H} as defined in RM Prescription 9; r
-- conforms to that heading; equivalently, r is of the corresponding relation type
-- (see RM Prescription 7). The degree of that heading {H} shall be the degree of
-- r, and the attributes and corresponding types of that heading {H} shall be the
-- attributes and corresponding declared attribute types of r.

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
    heading = undefined

instance Headed Relation where
    heading (Relation h _) = h


-- We want to be able to grab the attributes from a heading or a tuple or a relation.

attributes :: Headed a => a -> Set.Set AttributeName
--TODO: point free
attributes x = Set.fromList as
    where
        as = map fst pairs
        pairs = Set.toList aset
        aset = headingSet h
        h = heading x
        headingSet (Heading set) = set


-- We want to be able to ask the degree of a heading, a tuple or a relation.

class Degreed a where
    degree :: a -> Int

instance Degreed Heading where
    degree (Heading set) = Set.size set

instance Degreed Tuple where
    degree = degree . heading

instance Degreed Relation where
    degree = degree . heading

-- | Determine if a Headed entity conforms to the given Heading.
conforms :: Headed a => Heading -> a -> Bool
conforms h x = (heading x) == h

-- Missing types from above

type AttributeValue = String -- For now
type AttrSet = (Set.Set (AttributeName, TypeName))
type AttrValueSet = (Set.Set (AttributeName, AttributeValue))

-- Helpers for extracting the internal bits of things

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


-- If empty, "" else list of "AttrName TypeName," with no trailing comma,
-- separated by newlines
asKeyDefList :: Heading -> String
asKeyDefList (Heading set) | set == Set.empty = ""
                           | otherwise = "\n" ++ keyDefs ++ "\n"
                                where keyDefs = join ",\n" $ map fmt sortedKeys
                                      sortedKeys = sort $ Set.toList set
                                      fmt (a, b) = "    " ++ a ++ " " ++ b

instance Show Relation where
    show (Relation hd tuples) = top ++ bottom
        where top = "REAL RELATION {" ++ (asKeyDefList hd) ++ "}"
              prettyTuples = join ",\n" $ map prettyShow $ Set.toList tuples
              bottom = if (Set.size tuples > 0)
                           then " VALUES {\n" ++ prettyTuples ++ "\n}\n"
                           else ""
              prettyShow x = "    " ++ (show x)


-- Operations on relations

--insert :: Relation -> Relation -> Relation
--insert = undefined

insertTuple :: Relation -> Tuple -> Relation
insertTuple (Relation hd tups) t = Relation hd (Set.insert t tups)

