{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.WeightDecay
( weightDecay ) where

import Numeric.LinearAlgebra (Container, Vector, cmap, scale, sumElements)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.CostFunction (CostFunction(..))

square :: (Num a) => a -> a
square a = a * a

weightDecay :: (Container Vector a, Num a)
            => a
            -> CostFunction a
weightDecay coeff =
  let computeCost (BatchActivation input) =
        Right . (*coeff) . sumElements $
          cmap square input
      computeCost _ = Left "Weight decay expects batch activation"
      derivCost (BatchActivation input) =
        Right . BatchGradient . (scale coeff) $
          cmap (*2) input
      derivCost _ = Left "Weight decay expects batch activation"
  in CostFunction { computeCost
                  , derivCost
                  }
