module Main where
import Layer
import Models
import Numeric.LinearAlgebra

main :: IO()
main =
  do
    let mA = (2><2) [1.0, 2.0 , 3.0, 4.0] :: Matrix R
    let mB = (2><2) [2.0, 2.0, 2.0, 2.0] :: Matrix R
    let mC = mA + mB
    let mAString = show mC in
      putStrLn mAString
