module School.Unit.UnitParams
( UnitParams(..) ) where

import Numeric.LinearAlgebra (Matrix, Vector)

data UnitParams a =
   AffineParams { affineWeights :: Matrix a
                , affineBias :: Vector a
                }
 | EmptyParams deriving (Show)
