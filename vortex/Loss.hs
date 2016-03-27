module Loss where

import Numeric.LinearAlgebra


mse :: Matrix R -> Matrix R -> Matrix R
mse yMat tMat =
  let dyMat = yMat - tMat
      r = rows yMat
      c = cols yMat
      halfMat = (r >< c) [0.5 ..]::(Matrix R) in
  halfMat * dyMat * dyMat


