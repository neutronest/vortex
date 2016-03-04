module Layer where
import Numeric.LinearAlgebra
import Data.Dynamic
import Data.Random.Normal
import System.Random
import Utils

{-
class CommonLayer a where
  weightInit :: a -> Int -> Int -> Matrix R
  biasInit :: a -> Int -> Int -> Matrix R
  layerInit :: Int -> Int -> a
  forward :: a -> Matrix R
  backward :: a -> Matrix R -> Matrix R

genNormal :: (Random a, Floating a) => [a]
genNormal =
  let g = mkStdGen 1001 in
  normals g
-}
{-- LinearLayer --}

{-
data LinearLayer where
  LinearLayer :: forall a . CommonLayer a => a -> LinearLayer
-}

data LayerType = InputLayer
  | SigmoidLayer
  | LinearLayer
  | Softmax

data VLayer = VLayer {
  layerType :: LayerType,
  rowNum :: Int,
  colNum :: Int,
  weight :: Matrix R,
  bias :: Matrix R,
  val :: Matrix R
                     }

{-- Input Layer --}

inputLayerInit :: Int -> Int -> VLayer
inputLayerInit rowNum colNum = VLayer {
  layerType = InputLayer,
  rowNum = rowNum,
  colNum = colNum,
  weight = (rowNum >< colNum) genNormal,
  bias = (rowNum >< colNum) genNormal,
  val = (rowNum >< colNum) [0..]
                                }

forwardInputLayer :: Matrix R -> VLayer -> Matrix R
forwardInputLayer input inputLayer = (weight inputLayer) * input + (bias inputLayer)

backwardInputLayer :: VLayer -> Matrix R
backwardInputLayer inputLayer = weight inputLayer

{-- Linear Layer --}
linearLayerInit :: Int -> Int -> VLayer
linearLayerInit rowNum colNum = VLayer {
  layerType = LinearLayer,
  rowNum = rowNum,
  colNum = colNum,
  weight = (rowNum >< colNum) genNormal,
  bias = (rowNum >< colNum) genNormal,
  val = (rowNum >< colNum) [0..]
                                }

forwardLinearLayer :: Matrix R -> VLayer -> Matrix R
forwardLinearLayer input inputLayer = (weight inputLayer) * input + (bias inputLayer)

backwardLinearLayer :: VLayer -> Matrix R
backwardLinearLayer inputLayer = weight inputLayer


{-- Sigmoid Layer --}
sigmoidLayerInit :: Int -> Int -> VLayer
sigmoidLayerInit rowNum colNum = VLayer {
  layerType = SigmoidLayer,
  rowNum = rowNum,
  colNum = colNum,
  weight = (rowNum >< colNum) genNormal,
  bias = (rowNum >< colNum) genNormal,
  val = (rowNum >< colNum) [0..]
  }
  
forwardSigmoidLayer :: Matrix R -> VLayer -> Matrix R
forwardSigmoidLayer input layer =
  let linear = (weight layer) * input + (bias layer) in
  let sigmoid = 1.0 / (1.0 + (exp (- linear)) ) in
  sigmoid * (1.0 - sigmoid)

{-- Softmax Layer --}

