{-# LANGUAGE NamedFieldPuns #-}

module School.Train.GradientDescentPass
( gradientDescentPass ) where

import Conduit ((.|), ConduitM, mapMC)
import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (get, put)
import School.Train.AppTrain (AppTrain)
import School.Train.BackwardPass (backwardPass)
import School.Train.ForwardPass (forwardPass)
import School.Train.TrainState (TrainState(..))
import School.Train.UpdateParams (UpdateParams)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitGradient (UnitGradient(..))

updateStep :: UpdateParams a
           -> ConduitM (BackwardStack a)
                       (UnitGradient a)
                       (AppTrain a)
                       ()
updateStep update = mapMC $ \(_, grad) -> do
  currentState@TrainState{ iterationCount } <- get
  let iterState = currentState { iterationCount = iterationCount + 1 }
  let newState = update iterState
  either throwError put newState
  return grad

gradientDescentPass :: [Unit a]
                    -> CostFunction a
                    -> UpdateParams a
                    -> ConduitM (UnitActivation a)
                                (UnitGradient a)
                                (AppTrain a)
                                ()
gradientDescentPass units cost update =
    forwardPass units cost
 .| backwardPass units
 .| updateStep update
