module School.Train.GradientDescentPass
( gradientDescentPass ) where

import Conduit ((.|), ConduitM, await)
import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (get, put)
import School.Train.AppTrain (AppTrain)
import School.Train.BackwardPass (backwardPass)
import School.Train.ForwardPass (forwardPass)
import School.Train.UpdateParams (UpdateParams)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)

updateStep :: UpdateParams a
           -> ConduitM ()
                       ()
                       (AppTrain a)
                       ()
updateStep update = do
  _ <- await
  currentState <- get
  let newState = update currentState
  either throwError put newState

gradientDescentPass :: [Unit a]
                    -> CostFunction a
                    -> UpdateParams a
                    -> ConduitM (UnitActivation a)
                                ()
                                (AppTrain a)
                                ()
gradientDescentPass units cost update =
    forwardPass units cost
 .| backwardPass units
 .| updateStep update
