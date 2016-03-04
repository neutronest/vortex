module Models where

import Layer
import Numeric.LinearAlgebra
import Data.List


data Model a = Model [a]

addLayer :: Model a -> a -> Model a
addLayer (Model []) layer = Model [layer]
addLayer (Model l) layer = Model (l++[layer])

forwardInterface :: Matrix R -> VLayer -> Matrix R
forwardInterface input layer =
  let (VLayer {layerType, rowNum, colNum, weight, bias, val}) = layer in
  case layerType of
    InputLayer -> forwardInputLayer input layer
    SIgmoidLayer -> forwardSigmoidLayer input layer

forward :: Matrix R -> Model a -> Matrix R
forward input (Model []) = input
forward input (Model xs) = foldl forwardInterface input xs
