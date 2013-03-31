module Main (main) where

import Data.Relational.Tuple

main :: IO ()
main = do
    let x = RelVar
    putStrLn $ "hi" ++ (show x)

