module Models where

import Layer
import Numeric.LinearAlgebra

data Model a = Model [a]

addLayer :: CommonLayer a -> Model a -> Model a
addLayer layer model = model:layer
  
