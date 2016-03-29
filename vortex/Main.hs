module Main where
import Layer
import Models
import Numeric.LinearAlgebra
import ImageProcess
import Optimization
import Trainer
import Data.Vector.Storable (Vector, length, toList)


main :: IO()
main =
  do
    let inputLayer = linearLayerInit 2 2
    let sigmoidLayer = sigmoidLayerInit 2 2
    let outputLayer = linearLayerInit 2 1 in
      let model = Model [inputLayer, sigmoidLayer, outputLayer] in
      let input = (1><2)[1.0, 2.0]::Matrix R in
      let output = forward input model in
      print output



testImageRegression :: FilePath -> IO()
testImageRegression filepath = do
  imageM <- getImageInfo filepath
  case imageM of
    Nothing -> putStrLn "Nohthing ! Guess what happened ? :) " >> return ()
    Just imagePixel8 ->
      let rgbs = getImageRGBs imagePixel8
          w = getImageWidth imagePixel8
          h =  getImageHeight imagePixel8
          rgbsList = Data.Vector.Storable.toList rgbs
          (rlist, glist, blist) = genRGBList rgbsList
          Just rMat = genMatrixFromList rlist w h
          Just gMat = genMatrixFromList glist w h
          Just bMat = genMatrixFromList blist w h in
          --- test
          print "prepare the model!" >>
          let rinputLayer = linearLayerInit w h
              rreluLayer1 = reluLayerInit w h
              rreluLayer2 = reluLayerInit w h
              rreluLayer3 = reluLayerInit w h
              routputLayer=  linearLayerInit w h
              rmodel = Model [rinputLayer, rreluLayer1, rreluLayer2, rreluLayer3, routputLayer]
              ginputLayer = linearLayerInit w h
              greluLayer1 = reluLayerInit w h
              greluLayer2 = reluLayerInit w h
              greluLayer3 = reluLayerInit w h
              goutputLayer=  linearLayerInit w h
              gmodel = Model [ginputLayer, greluLayer1, greluLayer2, greluLayer3, goutputLayer]
              binputLayer = linearLayerInit w h
              breluLayer1 = reluLayerInit w h
              breluLayer2 = reluLayerInit w h
              breluLayer3 = reluLayerInit w h
              boutputLayer=  linearLayerInit w h
              bmodel = Model [binputLayer, breluLayer1, breluLayer2, breluLayer3, boutputLayer] in
              print "begin to train the model" >>
              let trainConfig = TrainConfig {
                    trainConfigRowNum=w,
                    trainConfigColNum=h,
                    trainConfigLearningRate=0.95,
                    trainConfigMomentum=1.0,
                    trainConfigDecay=0.95,
                    trainConfigEpsilon=0.00001,
                    trainConfigBatchSize=100,
                    trainConfigOptimization="adadelta"
                    } in
              trainSingleData rmodel trainConfig rMat 
