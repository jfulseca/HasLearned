{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Unit.UnitActivation
( ActivationSource
, UnitActivation(..)
, isApplyFail
) where

import Conduit (ConduitM)
import Numeric.LinearAlgebra (Container, Matrix, Vector)
import School.App.AppS (AppS)

data UnitActivation a =
    BatchActivation (Matrix a)
 |  ApplyFail String deriving (Show)

type ActivationSource a =
  ConduitM () (UnitActivation a) (AppS a) ()

instance (Container Vector a, Eq a, Num a) => Eq (UnitActivation a) where
  (BatchActivation a1) == (BatchActivation a2) =
     a1 == a2
  (ApplyFail _) == (ApplyFail _) = True
  _ == _ = False

isApplyFail :: UnitActivation a -> Bool
isApplyFail (ApplyFail _) = True
isApplyFail _ = False
