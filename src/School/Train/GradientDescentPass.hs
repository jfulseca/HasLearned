module School.Train.GradientDescentPass
( gradientDescentPass ) where

import Conduit ((.|), ConduitM)
import Control.Monad.State.Lazy (get, put)
import Data.Void (Void)
import School.Train.AppTrain (AppTrain)
import School.Train.BackwardPass (backwardPass)
import School.Train.ForwardPass (forwardPass)
import School.Train.UpdateParams (UpdateParams)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)

updateStep :: UpdateParams a
           -> ConduitM ()
                       Void
                       (AppTrain a)
                       ()
updateStep update = do
  currentState <- get
  put (update currentState)

gradientDescentPass :: [Unit a]
                    -> CostFunction a
                    -> UpdateParams a
                    -> ConduitM (UnitActivation a)
                                Void
                                (AppTrain a)
                                ()
gradientDescentPass units cost update =
    forwardPass units cost
 .| backwardPass units
 .| updateStep update
