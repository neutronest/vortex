module Utils where

import Data.Random.Normal
import Numeric.LinearAlgebra
import System.Random
--import Test.QuickCheck

genNormal :: (Random a, Floating a) => [a]
genNormal = let g = mkStdGen 1001 in
  normals g

testGenNormal :: (Random a, Floating a) => [a]
testGenNormal = let g = mkStdGen 1001 in
  take 10 (normals g)

getAverageMat :: Matrix R -> Double
getAverageMat mat =
  let r_f = fromIntegral $ rows mat
      c_f = fromIntegral $ cols mat
      sumMat = sumElements mat in
  sumMat / (r_f * c_f)

