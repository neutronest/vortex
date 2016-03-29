module Layer where
import Numeric.LinearAlgebra
import Utils

data LayerType =
  SigmoidLayer
  | LinearLayer
  | SoftmaxLayer
  | ReluLayer

data VLayer = VLayer {
  layerType :: LayerType,
  rowNum :: Int,
  colNum :: Int,
  weight :: Matrix R,
  bias :: Matrix R,
  val :: Matrix R,
  delta :: Matrix R
  }

{-- Linear Layer --}
linearLayerInit :: Int -> Int -> VLayer
linearLayerInit r c = VLayer {
  layerType = LinearLayer,
  rowNum = r,
  colNum = c,
  weight = (r >< c) genNormal,
  bias = (r >< c) genNormal,
  val = (r >< c) [0..]
                                }

forwardLinearLayer :: Matrix R -> VLayer -> Matrix R
forwardLinearLayer input inputLayer = (weight inputLayer) * input + (bias inputLayer)

backwardLinearLayer :: Matrix R -> VLayer -> Matrix R
backwardLinearLayer backInput inputLayer = backInput

{-- Sigmoid Layer --}
sigmoidLayerInit :: Int -> Int -> VLayer
sigmoidLayerInit r c = VLayer {
  layerType = SigmoidLayer,
  rowNum = r,
  colNum = c,
  weight = (r >< c) genNormal,
  bias = (r >< c) genNormal,
  val = (r >< c) [0..]
  }

getSigmoidMatrix :: Matrix R -> Matrix R
getSigmoidMatrix linear = 1.0 / (1.0 + (exp (- linear)))


  
forwardSigmoidLayer :: Matrix R -> VLayer -> Matrix R
forwardSigmoidLayer input layer =
  let linear = (weight layer) * input + (bias layer) in
  getSigmoidMatrix linear

backwardSigmoidLayer :: Matrix R -> VLayer -> Matrix R
backwardSigmoidLayer input layer =
  let linear = (weight layer) * input + (bias layer) in
  let sigmoid = getSigmoidMatrix linear in
  sigmoid * (1.0 - sigmoid)

{-- Softmax Layer --}
softmaxLayerInit :: Int -> Int -> VLayer
softmaxLayerInit r c = VLayer {
  layerType = SoftmaxLayer,
  rowNum = r,
  colNum = c,
  weight = (r >< c) genNormal,
  bias = (r >< c) genNormal,
  val = (r >< c) [0..]
                                        }

-- TODO: backward of softmax layer

{-- Relu Layer --}
reluLayerInit :: Int -> Int -> VLayer
reluLayerInit r c = VLayer {
  layerType = ReluLayer,
  rowNum = r,
  colNum = c,
  weight = (r >< c
           ) genNormal,
  bias = (r >< c) genNormal,
  val = (r >< c) [0..]
                                     }
forwardReluLayer :: Matrix R -> VLayer -> Matrix R
forwardReluLayer input layer =
  let flagMatrix = cmap (\x -> if x >= 0.0 then 1.0 else 0.0 ) (weight layer) in
  flagMatrix * input


backwardReluLayer :: Matrix R -> VLayer -> Matrix R
backwardReluLayer backInput layer =
  cmap (\x -> if x <= 0.0 then x else 0.0) backInput 

