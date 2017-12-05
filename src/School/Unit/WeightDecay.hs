{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.WeightDecay
( weightDecay ) where

import Conduit (mapC)
import Numeric.LinearAlgebra (Container, Vector, cmap, scale, sumElements)
import School.Train.TrainState (CostParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.CostFunction (CostFunction(..))

square :: (Num a) => a -> a
square a = a * a

weightDecay :: (Container Vector a, Num a)
            => a
            -> CostFunction a
weightDecay coeff =
  let computeCost (BatchActivation input) NoCostParams=
        Right . (*coeff) . sumElements $
          cmap square input
      computeCost _ _ = Left "Weight decay expects batch activation and no cost params"
      derivCost (BatchActivation input) NoCostParams =
        Right . BatchGradient . (scale coeff) $
          cmap (*2) input
      derivCost _ _ = Left "Weight decay expects batch activation and no cost params"
      setupCost = mapC pure
  in CostFunction { computeCost
                  , derivCost
                  , setupCost
                  }
