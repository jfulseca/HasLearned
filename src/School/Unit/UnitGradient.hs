module School.Unit.UnitGradient
( UnitGradient(..)
, isGradientFail
) where

import Numeric.LinearAlgebra (Matrix)

data UnitGradient a =
   BatchGradient (Matrix a)
 | GradientFail String deriving (Show)

isGradientFail :: UnitGradient a -> Bool
isGradientFail (GradientFail _) = True
isGradientFail _ = False
