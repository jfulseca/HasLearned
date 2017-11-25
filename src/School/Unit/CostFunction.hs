module School.Unit.CostFunction
( CostFunction(..) ) where

import School.Unit.UnitActivation (UnitActivation)
import School.Unit.UnitGradient (UnitGradient)

data CostFunction a =
  CostFunction { computeCost :: UnitActivation a -> a
               , derivCost :: UnitActivation a -> UnitGradient a
               }
