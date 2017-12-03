{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Unit.UnitParams
( UnitParams(..) ) where

import Numeric.LinearAlgebra (Container, Matrix, Vector)

data UnitParams a =
   AffineParams { affineWeights :: Matrix a
                , affineBias :: Vector a
                }
 | EmptyParams deriving (Show)

instance (Container Vector a, Eq a, Num a) => Eq (UnitParams a) where
  AffineParams { affineBias = b1, affineWeights = w1 } == AffineParams { affineBias = b2, affineWeights = w2 } =
    b1 == b2 && w1 == w2
  EmptyParams == EmptyParams = True
  _ == _ = False
