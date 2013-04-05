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

import Data.Brooks.Vals    ( DVal( StringVal ) )

import IO.Brooks.Database  ( bindName
                           , value
                           )

import IO.Brooks.Timothy   ( newDb
                           , withASE
                           )

import Language.Heidi.Lexer ( alexScanTokens )
import Language.Heidi.Parser hiding ( main )


-- heading :: Heading
-- heading = fromList [ ("username", "TEXT")
--                    , ("password", "TEXT")
--                    ]
--
-- emptyUsers :: Relation
-- emptyUsers = withHeading heading
--
-- john :: Tuple
-- john = headWith heading [ ("username", "john")
--                         , ("password", "letmein")
--                         ]
--
-- bob :: Tuple
-- bob = headWith heading [ ("username", "bob")
--                        , ("password", "letmein, also")
--                        ]
--
-- users :: Relation
-- users = insertTuple emptyUsers john
--
-- otherUsers :: Relation
-- otherUsers = insertTuple users bob


main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Args: " ++ (show args)
    case args of
        ["get", k]    -> getKey k
        ["set", k, v] -> setKey k v
        ["lex", fn] -> do
            stream <- readFile fn
            let result = lexMain stream
            putStrLn result
        ["lex"]     -> do
            stream <- getContents
            let result = lexMain stream
            putStrLn result
        _      -> putStrLn "<get k>, <set k v> or <lex [fn]>"
    putStrLn "Done"


getKey :: String -> IO ()
getKey k = do
    db <- newDb "test.db"
    putStrLn "yarp! getKey!"
    val <- withASE db $ \ase -> value ase k
    case val of
        (Just (StringVal s)) -> putStrLn $ "value is: " ++ (show s)
        (Just other) ->
            putStrLn $ "(extraordinary) value is: " ++ (show other)
        Nothing  -> putStrLn $ "no value."
    --close db

setKey :: String -> String -> IO ()
setKey k v = do
    db <- newDb "test.db"
    putStrLn "yarp! setKey!"
    withASE db $ \ase -> bindName ase k (StringVal v)
    --close db


lexMain :: String -> String
lexMain stream = show (alexScanTokens stream)


