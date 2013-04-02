module Data.Brooks.Vars where

-- TODO: these should be renamed to *Val and ..Brooks.Vals

import Data.Typeable        ( Typeable )
import Data.Relation.Types


data DVar = RelVar Relation
          | TupVar Tuple
          | BoolVar Bool
          | IntVar Int
          | FloatVar Float
          | DoubleVar Double
          | StringVar String
    deriving (Eq, Ord, Show, Typeable)

