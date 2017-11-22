module School.Unit.Unit
( Unit(..)
, UnitOutput ) where

import School.Unit.Constituents (Constituents)
import Numeric.LinearAlgebra (Matrix)

type UnitOutput a = Matrix a

data Unit a = Unit
  { deriv :: Constituents a -> UnitOutput a -> Constituents a
  , op :: Constituents a -> UnitOutput a
  }
