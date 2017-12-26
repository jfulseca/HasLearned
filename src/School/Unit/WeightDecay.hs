{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.WeightDecay
( weightDecay ) where

import Conduit (ConduitM, mapMC)
import Numeric.LinearAlgebra (Container, Matrix, Vector,
                              cmap, scale, sumElements)
import School.Types.Slinky (Slinky(..), slinkyAppend)
import School.Unit.CostFunction (CostFunction(..), defSetupCost)
import School.Unit.CostParams (CostParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
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

prepare :: (Monad m)
        => ConduitM (ForwardStack a)
                    (ForwardStack a)
                    m
                    ()
prepare = mapMC $ \(activations, cParams) ->
  return (activations, slinkyAppend NoCostParams cParams)

weightDecay :: (Container Vector a, Num a, Monad m)
            => a
            -> CostFunction a m
weightDecay coeff =
  CostFunction { computeCost = compute coeff
               , derivCost = deriv coeff
               , prepareCost = prepare
               , setupCost = defSetupCost
               } where
