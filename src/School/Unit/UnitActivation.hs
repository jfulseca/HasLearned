{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Unit.UnitActivation
( UnitActivation(..)
, isApplyFail
) where

import Numeric.LinearAlgebra (Container, Matrix, Vector)

data UnitActivation a =
    BatchActivation (Matrix a)
 |  ApplyFail String deriving (Show)

instance (Container Vector a, Eq a, Num a) => Eq (UnitActivation a) where
  (BatchActivation a1) == (BatchActivation a2) =
     a1 == a2
  (ApplyFail _) == (ApplyFail _) = True
  _ == _ = False

isApplyFail :: UnitActivation a -> Bool
isApplyFail (ApplyFail _) = True
isApplyFail _ = False
