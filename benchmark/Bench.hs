module Main (main) where

import Criterion.Main ( bgroup
                      , defaultMain
                      )
import qualified BrooksBench

main :: IO ()
main = defaultMain [ bgroup "Brooks" BrooksBench.benchmarks ]
