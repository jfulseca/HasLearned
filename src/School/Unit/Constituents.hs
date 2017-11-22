module School.Unit.Constituents
( Constituents(..) ) where

import Numeric.LinearAlgebra (Matrix, Vector)

data Constituents a =
  AffineConstituents { weightMatrix :: Matrix a
                     , inputMatrix :: Matrix a
                     , biasVector :: Vector a
                     }
