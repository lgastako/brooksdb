module Data.Brooks.Vars where

-- Todo these should be renamed to *Val and ..Brooks.Vals

import Data.Relation.Types

data DVar = RelVar Relation
          | TupVar Tuple
          | BoolVar Bool
          | IntVar Int
          | FloatVar Float
          | DoubleVar Double
          | StringVar String
    deriving (Eq, Ord, Show)

