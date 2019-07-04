module Main where

import Data.Array.Accelerate.Test.NoFib (nofib)
import Data.Array.Accelerate.LLVM.Native (runN)
import Data.Array.Accelerate.Debug (beginMonitoring)

main :: IO ()
main = do
  beginMonitoring
  nofib runN

