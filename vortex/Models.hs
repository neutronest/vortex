module Models where

import Layer
import Numeric.LinearAlgebra
import Data.List
import Loss

data Model a = Model [a]

addLayer :: Model a -> a -> Model a
addLayer (Model []) layer = Model [layer]
addLayer (Model l) layer = Model (l++[layer])

genParamsList :: Model VLayer -> [Matrix R]
genParamsList (Model []) = []
genParamsList m =
  let genModelParams' res (Model []) = reverse res
      genModelParams' res (Model (mx:mxs)) =
        genModelParams' ((weight mx):res) (Model mxs) in
  genModelParams' [] m


-- update the model's params from the params list
updateModelParams :: Model VLayer -> [Matrix R] -> Model VLayer
updateModelParams (Model []) _ = Model []
updateModelParams m p  =
  let updateModelParams' (Model res) (Model []) _ = Model (reverse res)
      updateModelParams' (Model res) (Model (mx:mxs)) (px:pxs) =
        let newParams = mx {weight=px} in
        updateModelParams' (Model (newParams:res)) (Model mxs) pxs
  in updateModelParams' (Model []) m p

genGParamsList :: Model VLayer -> [Matrix R]
genGParamsList (Model []) = []
genGParamsList l =
  let genModelGParams' res (Model []) = reverse res
      genModelGParams' res (Model (mx:mxs)) =
        let gp = ( (rowNum mx)  >< (colNum mx)) [0..]::(Matrix R) in
        genModelGParams' (gp:res) (Model mxs) in
  genModelGParams' [] l

--updateModelGParams :: Model VLayer -> [Matrix R] -> Model VLayer
-- update the gparams list from the model
updateGParamsList :: Model VLayer -> [Matrix R] -> [Matrix R]
updateGParamsList (Model []) _ = []
updateGParamsList l gpl =
  let updateModelGParams' res (Model []) _ = reverse res
      updateModelGParams res (Model (mx:mxs)) (gpx:gpxs) =
        let deltaUp = delta mx in
        updateModelGParams' ((deltaUp+gpx):res) (Model mxs) gpxs in
  updateModelGParams' [] l gpl
  

forwardInterface :: Matrix R -> VLayer -> Matrix R
forwardInterface input layer =
  case (layerType layer) of
    LinearLayer -> forwardLinearLayer input layer
    SigmoidLayer -> forwardSigmoidLayer input layer
    --SoftmaxLayer -> forwardSoftmaxLayer input layer
    ReluLayer -> forwardReluLayer input layer


backwardInterface :: VLayer -> Matrix R -> Matrix R
backwardInterface layer output =
  case (layerType layer) of
    LinearLayer -> backwardLinearLayer output layer
    SigmoidLayer -> backwardSigmoidLayer output layer
    ReluLayer -> backwardReluLayer output layer

-- forward process of the neural network model
forward :: Matrix R -> Model VLayer -> Matrix R
forward input (Model []) = input
forward input (Model xs) = foldl forwardInterface input xs

-- the backpropagate algorithm of Neural Network
-- update the delta of each layer from end to start
backward :: String -> Matrix R -> Matrix R -> Model VLayer -> Model VLayer
backward _ _ _ (Model []) = Model []
backward lossType yMat tMat l =
    let loss = getLoss lossType yMat tMat in
    let backward' (Model res) (Model []) _ = Model (reverse res)
        backward' (Model res) (Model (mx:mxs)) dw =
          let dwUp = backwardInterface mx dw
              layerUp = mx {delta=dwUp} in
          backward' (Model (layerUp:res)) (Model mxs) dwUp in
    backward' (Model []) l loss
