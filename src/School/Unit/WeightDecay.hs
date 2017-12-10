{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.WeightDecay
( weightDecay ) where

import Conduit (mapC)
import Numeric.LinearAlgebra (Container, Matrix, Vector,
                              cmap, scale, sumElements)
import School.Types.Slinky (Slinky(..))
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.CostParams (CostParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))

square :: (Num a) => a -> a
square a = a * a

weight :: (Container Vector a, Num a)
       => a -> Matrix a -> a
weight coeff input =
  (*coeff) . sumElements $ cmap square input

weightDeriv :: (Container Vector a, Num a) 
            => a -> Matrix a -> UnitGradient a
weightDeriv coeff input =
  BatchGradient . (scale coeff) $
    cmap (*2) input

errorMsg :: String
errorMsg = "Weight decay expects batch activation and no cost params"

compute :: (Container Vector a, Num a)
        => a
        -> UnitActivation a
        -> Slinky CostParams
        -> Either String a
compute coeff
        (BatchActivation input)
        (SNode NoCostParams _) =
  Right $ weight coeff input
compute coeff
        (BatchActivation input)
        SNil =
  Right $ weight coeff input
compute _ _ _ = Left errorMsg

deriv :: (Container Vector a, Num a)
      => a
      -> UnitActivation a
      -> Slinky CostParams
      -> Either String (UnitGradient a)
deriv coeff
      (BatchActivation input)
      (SNode NoCostParams _) =
  Right $ weightDeriv coeff input
deriv coeff
      (BatchActivation input)
      SNil =
  Right $ weightDeriv coeff input
deriv _ _ _ = Left errorMsg

weightDecay :: (Container Vector a, Num a)
            => a
            -> CostFunction a
weightDecay coeff =
  CostFunction { computeCost = compute coeff
               , derivCost = deriv coeff
               , setupCost = mapC pure
               } where
