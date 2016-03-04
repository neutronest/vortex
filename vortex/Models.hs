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
  let (VLayer {layerType=layerType,
              rowNum=_,
              colNum=_,
              weight=_,
              bias=_,
              val=_}) = layer in
  case layerType of
    LinearLayer -> forwardInputLayer input layer
    InputLayer -> forwardInputLayer input layer
    SigmoidLayer -> forwardSigmoidLayer input layer
    --SoftmaxLayer -> forwardSoftmaxLayer input layer

forward :: Matrix R -> Model VLayer -> Matrix R
forward input (Model []) = input
forward input (Model xs) = foldl forwardInterface input xs
