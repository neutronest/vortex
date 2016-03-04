module Main where
import Layer
import Models
import Numeric.LinearAlgebra


main :: IO()
main =
  do
    let inputLayer = inputLayerInit 2 2
    let sigmoidLayer = sigmoidLayerInit 2 2
    let outputLayer = linearLayerInit 2 2 in
      let model = Model [inputLayer, sigmoidLayer, outputLayer] in
      let input = (1><2)[1.0, 2.0]::Matrix R in
      let output = forward input model in
      print output
      
