{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.WeightDecay
( weightDecay ) where

import Conduit (mapC)
import Numeric.LinearAlgebra (Container, Vector, cmap, scale, sumElements)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.CostParams (CostParams(..), LinkedParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))

square :: (Num a) => a -> a
square a = a * a

weightDecay :: (Container Vector a, Num a)
            => a
            -> CostFunction a
weightDecay coeff =
  let computeCost (BatchActivation input) (Node NoCostParams _) =
        Right . (*coeff) . sumElements $
          cmap square input
      computeCost _ _ = Left "Weight decay expects batch activation and no cost params"
      derivCost (BatchActivation input) (Node NoCostParams _) =
        Right . BatchGradient . (scale coeff) $
          cmap (*2) input
      derivCost _ _ = Left "Weight decay expects batch activation and no cost params"
      setupCost = mapC pure
  in CostFunction { computeCost
                  , derivCost
                  , setupCost
                  }
