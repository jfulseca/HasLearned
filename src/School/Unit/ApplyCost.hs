{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, mapMC)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain)
import School.Types.LiftResult (liftResult)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)

applyCost :: CostFunction a
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppTrain a)
                      ()
applyCost costFunc = mapMC $ \(activations, cParams) -> do
  when (length activations < 1) $ throwError "No activations in applyCost"
  let activation = head activations
  cost <- liftResult $ computeCost costFunc activation cParams
  let grad = derivCost costFunc activation cParams
  either throwError
    (\g -> return (tail activations, g, cost))
    grad
