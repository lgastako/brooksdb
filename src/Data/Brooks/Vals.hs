module Data.Brooks.Vals where

-- TODO: these should be renamed to *Val and ..Brooks.Vals

import Data.Typeable        ( Typeable )
import Data.Relation.Types


data DVal = RelVal Relation
          | TupVal Tuple
          | BoolVal Bool
          | IntVal Int
          | FloatVal Float
          | DoubleVal Double
          | StringVal String
    deriving (Eq, Ord, Show, Typeable)

