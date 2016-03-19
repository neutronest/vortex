module Optimization where

import Numeric.LinearAlgebra

class Optimize a where
  --optimizeInit :: a -> Double -> Double -> (Int, Int)  -> a
  paramUpdate :: a -> a

data SGD = SGD {
  sgdLearningRate :: Double,
  sgdDecay :: Double,
  sgdMomentum :: Double,
  sgdParams :: [Matrix R],
  sgdGparams :: [Matrix R],
  sgdDeltaPre :: [Matrix R],
  sgdNTimes :: Double
                 } 

instance Optimize SGD where
  paramUpdate sgd =
    let learningRate = sgdLearningRate sgd in
    let decay = sgdDecay sgd in
    let momentum = sgdMomentum sgd in
    let params = sgdParams sgd in
    let gparams = sgdGparams sgd in
    let deltaPre = sgdDeltaPre sgd in
    let nTimes = sgdNTimes sgd in
    let _sgd (px:pxs) (gx: gxs) (dx:dxs) paUpdate deUpdate gaZeros  =
          let r = rows gx in
          let c = cols gx in
          let lrMat = (r >< c) [learningRate..]:: (Matrix R) in
          let nTimesMat = (r >< c) [nTimes..] :: (Matrix R) in
          let momMat = (r >< c) [momentum..] :: (Matrix R) in
          let gUp = gx * lrMat / nTimesMat in
          let ugd = momMat * dx - gx in
          let pu = px + ugd in _sgd pxs gxs dxs (paUpdate++[pu]) (deUpdate++[ugd]) (gaZeros++[(r >< c) [0..]])
        _sgd [] [] [] paUpdate deUpdate gaZeros = SGD {
          sgdLearningRate=learningRate,
          sgdDecay=decay,
          sgdMomentum=momentum,
          sgdParams=paUpdate,
          sgdGparams=gaZeros,
          sgdDeltaPre=deUpdate,
          sgdNTimes=0
          } in
    _sgd params gparams deltaPre [] [] []
data Adadelta = Adadelta {
  adaDecay :: Double,
  adaEpsilon :: Double,
  adaNTimes :: Int,
  adaGparams :: [Matrix R],
  adaAccGrad :: [Matrix R],
  adaAccDelta :: [Matrix R]
                         }

{-
instance Optimize Adadelta where
  paramUpdate adadelta params =
    let gs = map (\g -> g / (nTimes adadelta)) (gparams adadelta) in
    let accGradUpdate = map (\accg -> (decay adadelta) * accg + (1 - (decay adadelta) * g * g)) in
    let ugd = 
-}    
