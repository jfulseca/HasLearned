{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, mapMC)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (get)
import School.Train.AppTrain (AppTrain)
import School.Train.StateFunctions (putCost)
import School.Train.TrainState (TrainState(..))
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)

applyCost :: CostFunction a
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppTrain a)
                      ()
applyCost costFunc = mapMC $ \stack -> do
  when (length stack < 1) $ throwError "No activations in applyCost"
  let activation = head stack
  TrainState { costParams } <- get
  let cost = computeCost costFunc activation costParams
  either throwError putCost cost
  let grad = derivCost costFunc activation costParams
  either throwError
    (\g -> return (tail stack, g))
    grad
