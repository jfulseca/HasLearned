{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Unit.UnitGradient
( UnitGradient(..)
, isGradientFail
) where

import Numeric.LinearAlgebra (Container, Matrix, Vector)

data UnitGradient a =
   BatchGradient (Matrix a)
 | GradientFail String deriving (Show)

instance (Container Vector a, Num a, Eq a) => Eq (UnitGradient a) where
  (BatchGradient g1) == (BatchGradient g2) =
    g1 == g2
  (GradientFail _) == (GradientFail _) = True
  _ == _ = False

isGradientFail :: UnitGradient a -> Bool
isGradientFail (GradientFail _) = True
isGradientFail _ = False
