module Temp where

import Data.Relation.Types ( Relation
                           , Tuple
                           , Heading
                           , headWith
                           , withHeading
                           , fromList
                           , insertTuple
                           )


--db = newDb

heading :: Heading
heading = fromList [ ("username", "TEXT")
                   , ("password", "TEXT")
                   ]

emptyUsers :: Relation
emptyUsers = withHeading heading

john :: Tuple
john = headWith heading [ ("username", "john")
                        , ("password", "letmein")
                        ]

bob :: Tuple
bob = headWith heading [ ("username", "bob")
                       , ("password", "letmein, also")
                       ]

users :: Relation
users = insertTuple emptyUsers john

otherUsers :: Relation
otherUsers = insertTuple users bob

