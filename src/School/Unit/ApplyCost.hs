{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, mapMC)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain)
import School.Types.LiftResult (liftResult)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)
import School.Utils.Monad (headMonad)

applyCost :: CostFunction a (AppTrain a)
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppTrain a)
                      ()
applyCost costFunc = mapMC $ \(activations, cParams) -> do
  activation <- headMonad activations "No activations to applyCost"
  cost <- liftResult $ computeCost costFunc activation cParams
  let grad = derivCost costFunc activation cParams
  either throwError
    (\g -> return (tail activations, g, cost))
    grad
