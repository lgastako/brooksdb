-- benchmark/BrooksBench.hs
module BrooksBench (benchmarks) where

import Criterion ( Benchmark
                 , bench
                 , nf
                 )
--import Brooks ( husk )

benchmarks :: [Benchmark]
benchmarks = [ --bench "brooksdb" (nf (const brooks) ())
             ]
