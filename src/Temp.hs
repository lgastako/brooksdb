module Temp where

import Data.Relation.Types ( Relation
                           , withHeading
                           , fromList
                           )

--db = newDb

users :: Relation
users = withHeading $ fromList [ ("username", "TEXT")
                               , ("password", "TEXT")
                               ]



