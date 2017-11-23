module School.Unit.UnitGradient
( UnitGradient(..) ) where

import Numeric.LinearAlgebra (Matrix)

data UnitGradient a =
   BatchGradient (Matrix a)
 | GradientFail String
