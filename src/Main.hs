module Main (main) where

import Data.Relation.Types ( Relation
                           , Tuple
                           , Heading
                           , headWith
                           , withHeading
                           , fromList
                           , insertTuple
                           )

import IO.Brooks.Database ( Database )

import IO.Brooks.Timothy ( AcidStateEngine
                         , newDb
                         )


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


main :: IO ()
main = do
    putStrLn "\n"
    putStrLn $ show users
    putStrLn "\n"
    putStrLn $ show otherUsers
    putStrLn "\n"

    let db = newDb "test.db"
    --close db
    putStrLn "whatever"

