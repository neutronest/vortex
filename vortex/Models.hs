module Models where

import Layer
import Numeric.LinearAlgebra
import Data.List


data Model a = Model [a]

addLayer :: Model a -> a -> Model a
addLayer (Model []) layer = Model [layer]
addLayer (Model l) layer = Model (l++[layer])

genModelParams :: Model VLayer -> [Matrix R]
genModelParams (Model []) = []
genModelParams (Model l) =
  let genModelParams' res (Model []) = reverse res
      genModelParams' res (Model (x:xs)) =
        let (VLayer {layerType=_,
                    rowNum=_,
                    colNum=_,
                    weight=w,
                    bias=_,
                    val=_,
                    delta=_}) = x in
        genModelParams (w x):res xs in
  genModelParams' [] l

updateModelParams :: Model VLayer -> [Matrix R] -> Model VLayer
updateModelParams (Model []) _ = Model []
updateModelParams l p  =
  let updateModelParams' res (Model []) _ = reverse res
      updateModelParams res (Model mx:mxs) (px:pxs) =  
        let (VLayer {layerType=t,
                     rowNum=c,
                     colNum=l,
                     weight=w,
                     bias=b,
                     val=v,
                     delta=d}) = mx
            newParams = VLayer {
              layerType=t,
              rowNum=c,
              colNum=l,
              weight=px,
              bias=b,
              val=v,
              delta=d
              } in updateModelParams' (newParams:res) mxs pxs
  in updateModelParams' (Model []) l p
  

genModelGParams :: Model VLayer -> [Matrix R]
genModelGParams (Model []) = []
genModelGParams (Model l) =
  let genModelGParams' res (Model []) = reverse res
      genModelGParams' res (Model x:xs) =
        let (VLayer {layerType=_,
                    rowNum=r,
                    colNum=c,
                    weight=_,
                    bias=_,
                    val=_,
                    delta=_}) = x in
        let gp = (r >< c) [0..]::(Matrix R) in
        genModelGParams' gp:res xs in
  genModleGParams' [] l

--updateModelGParams :: Model VLayer -> [Matrix R] -> Model VLayer


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
    ReluLayer -> forwardReluLayer input layer

backwardInterface :: VLayer -> Matrix R -> Matrix R
backwardInterface layer output =
  let (VLayer { layerType=layerType,
                rowNum=_,
                colNum=_,
                weight=_,
                bias=_,
                val=_
              }) = layer in
  case layerType of
    LinearLayer -> backwardLinearLayer output layer
    InputLayer -> backwardInputLayer output layer
    SigmoidLayer -> backwardSigmoidLayer output layer
    ReluLayer -> backwardReluLayer output layer

-- forward process of the neural network model
forward :: Matrix R -> Model VLayer -> Matrix R
forward input (Model []) = input
forward input (Model xs) = foldl forwardInterface input xs

backward :: Model VLayer -> Matrix R -> Matrix R
backward (Model []) output = output
backward (Model xs) output = foldr backwardInterface output xs 

backPropagate :: Model VLayer 
