module Main (main) where

--import Data.Relation.Types

import Temp ( users )


main :: IO ()
main = do
    putStrLn $ show users

