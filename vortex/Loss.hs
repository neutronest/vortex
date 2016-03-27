module Loss where

import Numeric.LinearAlgebra


getLoss :: String -> Matrix R -> Matrix R -> Matrix R
getLoss lossType yMat tMat =
  case lossType of
    "mse" -> mse yMat tMat
    otherwise -> yMat -- Warning: Never Used Necessery!

mse :: Matrix R -> Matrix R -> Matrix R
mse yMat tMat =
  let dyMat = yMat - tMat
      r = rows yMat
      c = cols yMat
      halfMat = (r >< c) [0.5 ..]::(Matrix R) in
  halfMat * dyMat * dyMat


