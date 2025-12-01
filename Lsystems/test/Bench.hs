{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
import Test.Tasty.Bench ( defaultMain, Benchmark, bgroup, bench, nf, env )
import GHC.Generics
import Control.DeepSeq

import IC.Colour
import LSystems
import Examples

main :: IO ()
main = defaultMain [benchmarks]

benchmarks :: Benchmark
benchmarks = bgroup "lsystems"
  [
  ]

-- 'ere be Dragons
-- This stuff is used to allow for the strict evaluation of Commands and Colours
-- Don't worry about how this works :)
deriving instance Generic Command
deriving instance NFData Command
deriving instance Generic Colour
deriving instance NFData Colour
