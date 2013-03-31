module Main (main) where

import Data.Relational.Types

main :: IO ()
main = do
    let x = 5 :: Int
    putStrLn $ "hi " ++ (show x)

