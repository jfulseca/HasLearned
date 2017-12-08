{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, mapMC)
import Control.Monad (when)
import Control.Monad.State.Lazy (get)
import School.App.AppS (AppS, throw)
import School.Train.AppTrain (putCost)
import School.Train.TrainState (TrainState(..))
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)

applyCost :: CostFunction a
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppS a)
                      ()
applyCost costFunc = mapMC $ \stack -> do
  when (length stack < 1) $ throw "No activations in applyCost"
  let activation = head stack
  TrainState { costParams } <- get
  let cost = computeCost costFunc activation costParams
  either throw putCost cost
  let grad = derivCost costFunc activation costParams
  either throw
    (\g -> return (tail stack, g))
    grad
