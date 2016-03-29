module Trainer where

import Numeric.LinearAlgebra
import Models
import Layer
import Optimization
import Data.Foldable
import Control.Monad.IO.Class
import Utils

data TrainConfig = TrainConfig {
  trainConfigRowNum :: Int,
  trainConfigColNum :: Int,
  trainConfigLearningRate :: Double,
  trainConfigMomentum :: Double,
  trainConfigDecay :: Double,
  trainConfigEpsilon :: Double,
  trainConfigBatchSize :: Double,
  trainConfigOptimization:: String
                               }

trainSingleData :: Model VLayer -> TrainConfig -> Matrix R -> IO ()
trainSingleData (Model []) _ _ = print "No models !" >> return ()
trainSingleData neuModel trainConfig inputData =
  let r = trainConfigRowNum trainConfig
      c = trainConfigColNum trainConfig
      paramsList = genParamsList neuModel
      gparamsList = genGParamsList neuModel
      accGradList = accMatrixInit paramsList
      accDeltaList = accMatrixInit paramsList
      optimize = Adadelta {
                adaDecay = trainConfigDecay trainConfig,
                adaEpsilon = trainConfigEpsilon trainConfig,
                adaBatchSize = (trainConfigBatchSize trainConfig),
                adaParams = paramsList,
                adaGparams = gparamsList,
                adaAccGrad = accGradList,
                adaAccDelta = accDeltaList
                            } in
  -- begin to train
  forM_ [1..10000] $ \idx -> do
    --print idx
    let output = forward inputData neuModel in
      --print "backpropagate the model" >>
      let neuModel = backward "mse" output inputData neuModel in
      --print "neumodel" >>
      let gparamsList = updateGParamsList neuModel gparamsList in
      --print "continue" >> 
      if mod idx 100 == 0 && idx >= 100 then
        -- update the params in optimize
        -- update the params in model
        let optimize = paramUpdate optimize
            paramsList = adaParams optimize
            neuModel = updateModelParams neuModel paramsList in
        print (getAverageMat output) >> 
        return()
      else return()
