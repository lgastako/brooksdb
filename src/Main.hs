module Main (main) where

import Data.Relational.Types

main :: IO ()
main = do
    let x = RelVar
    putStrLn $ "hi" ++ (show x)

