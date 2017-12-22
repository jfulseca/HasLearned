{-# LANGUAGE NamedFieldPuns #-}

module School.Train.GradientDescentPass
( gradientDescentPass ) where

import Conduit ((.|), ConduitM, mapMC)
import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (get, put)
import School.Train.AppTrain (AppTrain)
import School.Train.BackwardPass (backwardPass)
import School.Train.ForwardPass (forwardPass)
import School.Train.IterationHandler (BackwardConduit)
import School.Train.TrainState (TrainState(..))
import School.Train.UpdateParams (UpdateParams)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitBackward (BackwardStack)

updateStep :: UpdateParams a
           -> BackwardConduit a
updateStep update = mapMC $ \(stack, grad, cost) -> do
  state@TrainState{ iterationCount } <- get
  let tryUpdate = update state
  either throwError
         (\state' -> do
            let state'' = state' { iterationCount = iterationCount + 1
                                 , paramDerivs = []
                                 }
            put state'')
         tryUpdate
  return (stack, grad, cost)

gradientDescentPass :: [Unit a]
                    -> CostFunction a (AppTrain a)
                    -> UpdateParams a
                    -> ConduitM (ForwardStack a)
                                (BackwardStack a)
                                (AppTrain a)
                                ()
gradientDescentPass units cost update =
    forwardPass units cost
 .| backwardPass units
 .| updateStep update
