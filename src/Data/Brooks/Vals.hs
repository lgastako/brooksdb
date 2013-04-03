{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Data.Brooks.Vals where

import Data.Typeable        ( Typeable )
import Data.Relation.Types

import Data.SafeCopy        ( base
                            , deriveSafeCopy
                            )

data DVal = RelVal Relation
          | TupVal Tuple
          | BoolVal Bool
          | IntVal Int
          | FloatVal Float
          | DoubleVal Double
          | StringVal String
    deriving (Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''DVal)
