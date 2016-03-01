{-# LANGUAGE TypeSynonymInstances #-}
module Layer where

import Numeric.LinearAlgebra


class CommonLayer a where
  forward :: a -> a
  backward :: a -> a
  getVal :: a -> a
  getGrad :: a -> a
  getWeight :: a -> a
  getBias :: a -> a


data SigmoidLayer = SigmoidLayer {
  weight :: Matrix R,
  bias :: Matrix R,
  grad :: Matrix R,
  val :: Matrix R,
  size :: (Integer, Integer)
                                   } deriving (Show)  

