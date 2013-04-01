module Main (main) where

--import Data.Relation.Types

import Temp ( users, otherUsers )


main :: IO ()
main = do
    putStrLn "\n"
    putStrLn $ show users
    putStrLn "\n"
    putStrLn $ show otherUsers
    putStrLn "\n"
