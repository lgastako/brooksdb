module Main (main) where

import System.Environment  ( getArgs )

import Data.Relation.Types ( Relation
                           , Tuple
                           , Heading
                           , headWith
                           , withHeading
                           , fromList
                           , insertTuple
                           )

import Data.Brooks.Vals

import IO.Brooks.Database  ( bindName
                           , value
                           )

import IO.Brooks.Timothy   ( newDb
                           , withASE
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
    args <- getArgs
    putStrLn $ "Args: " ++ (show args)

    putStrLn "\n"
    putStrLn $ show users
    putStrLn "\n"
    putStrLn $ show otherUsers
    putStrLn "\n"

    db <- newDb "test.db"
    putStrLn "yarp!"
    --withASE db $ \ase -> bindName ase "foo" (StringVal "bar")
    val <- withASE db $ \ase -> value ase "foo"
    case val of
        (Just x) -> putStrLn $ "value is: " ++ (show val)
        Nothing -> putStrLn $ "no value."
    --close db
    putStrLn "done"

