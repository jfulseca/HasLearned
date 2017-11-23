module School.Unit.UnitActivation
( UnitActivation(..)
, isApplyFail
) where

import Numeric.LinearAlgebra (Matrix)

data UnitActivation a =
    BatchActivation (Matrix a)
 |  ApplyFail String deriving (Show)

isApplyFail :: UnitActivation a -> Bool
isApplyFail (ApplyFail _) = True
isApplyFail _ = False
