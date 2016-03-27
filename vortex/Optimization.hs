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
  sgdBatchSize :: Double
                 } 

instance Optimize SGD where
  paramUpdate sgd =
    let learningRate = sgdLearningRate sgd in
    let decay = sgdDecay sgd in
    let momentum = sgdMomentum sgd in
    let params = sgdParams sgd in
    let gparams = sgdGparams sgd in
    let deltaPre = sgdDeltaPre sgd in
    let batchSize = sgdBatchSize sgd in
    let _sgd (px:pxs) (gx: gxs) (dx:dxs) paUpdate deUpdate gaZeros  =
          let r = rows gx in
          let c = cols gx in
          let lrMat = (r >< c) [learningRate..]:: (Matrix R) in
          let batchSizeMat = (r >< c) [batchSize..] :: (Matrix R) in
          let momMat = (r >< c) [momentum..] :: (Matrix R) in
          let gUp = gx * lrMat / batchSizeMat in
          let ugd = momMat * dx - gx in
          let pu = px + ugd in _sgd pxs gxs dxs (paUpdate++[pu]) (deUpdate++[ugd]) (gaZeros++[(r >< c) [0..]])
        _sgd [] [] [] paUpdate deUpdate gaZeros = SGD {
          sgdLearningRate=learningRate,
          sgdDecay=decay,
          sgdMomentum=momentum,
          sgdParams=paUpdate,
          sgdGparams=gaZeros,
          sgdDeltaPre=deUpdate,
          sgdBatchSize=0
          } in
    _sgd params gparams deltaPre [] [] []


data Adadelta = Adadelta {
  adaDecay :: Double,
  adaEpsilon :: Double,
  adaBatchSize :: Double,
  adaParams :: [Matrix R],
  adaGparams :: [Matrix R],
  adaAccGrad :: [Matrix R],
  adaAccDelta :: [Matrix R]
                         }


instance Optimize Adadelta where
  paramUpdate ada =
    let decay = adaDecay ada in
    let epsilon = adaEpsilon ada in
    let batchSize = adaBatchSize ada in
    let params = adaParams ada in
    let gparams = adaGparams ada in
    let accGrad = adaAccGrad ada in
    let accDelta = adaAccDelta ada in
    let _ada (px:pxs) (gx:gxs) (accgx:accgxs) (accdx:accdxs) paUpdate gradUpdate deltaUpdate gaZeros =
          let r = rows px in
          let c = cols px in
          let decayMat = (r >< c) [decay ..] :: (Matrix R) in
          let epsilonMat = (r >< c) [epsilon ..] :: (Matrix R) in
          let batchSizeMat = (r >< c) [batchSize ..] :: (Matrix R) in
          let oneMat = (r >< c) [1.0 ..]:: (Matrix R) in
          let gUp = accgx / batchSizeMat in 
          let gradUp = decayMat * accgx + (oneMat - decayMat) * gUp * gUp in
          let ugd = (cmap (\x -> -(sqrt x)) (accdx + epsilonMat)) /  (cmap (\x -> sqrt x) (accgx + epsilonMat)) * gUp in
          let deltaUp = decayMat * accdx + (oneMat - decayMat) * ugd * ugd in
          let pu = px + ugd in
          _ada pxs gxs accgxs accdxs (pu:paUpdate) (gradUp:gradUpdate) (deltaUp:deltaUpdate) (((r >< c)[0.0 ..]):gaZeros)
        _ada [] [] [] [] paUpdate gradUpdate deltaUpdate gaZeros = Adadelta {
          adaDecay=decay,
          adaEpsilon=epsilon,
          adaBatchSize=batchSize,
          adaParams=(reverse paUpdate),
          adaGparams=(reverse gaZeros),
          adaAccGrad=(reverse gradUpdate),
          adaAccDelta=(reverse deltaUpdate)
                                                                        }
    in _ada params gparams accGrad accDelta [] [] [] [] 
