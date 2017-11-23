module School.Unit.UnitActivation
( UnitActivation(..) ) where

import Numeric.LinearAlgebra (Matrix)

data UnitActivation a =
    BatchActivation (Matrix a)
 |  ApplyFail String deriving (Show)
