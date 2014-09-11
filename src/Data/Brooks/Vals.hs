{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Data.Brooks.Vals where

import Data.Map
import Data.Typeable        ( Typeable )
import Data.Relation.Types

import Data.SafeCopy        ( base
                            , deriveSafeCopy
                            )

--type WrappedDVal = (Int, DVal)

-- TODO: Rename to BTree or generalize to N-leaves
data Tree a = Leaf a
--data (Eq a, Ord a, Show a) => Tree a = Leaf a
       | Tree (Tree a) (Tree a)
    deriving (Eq, Ord, Show, Typeable)

instance Functor Tree where
    fmap f (Leaf x) = (Leaf (f x))
    fmap f (Tree left right) = (Tree (fmap f left) (fmap f right))


data DVal = RelVal Relation
          | TupVal Tuple
          | BoolVal Bool
          | IntVal Int
          | FloatVal Float
          | DoubleVal Double
          | StringVal String
          | TreeVal (Tree DVal)
          | MapVal (Map DVal DVal)
    deriving (Eq, Ord, Show, Typeable)


$(deriveSafeCopy 0 'base ''DVal)
$(deriveSafeCopy 0 'base ''Tree)
