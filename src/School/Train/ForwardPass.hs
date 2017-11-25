module School.Train.ForwardPass
( forwardPass ) where

import Conduit ((.|), ConduitM, await, mapM_C, yieldM)
import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (put)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (TrainState)
import School.Unit.ApplyCost (applyCost)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack, unitForward)

initForward :: (TrainState a)
            -> ConduitM (UnitActivation a)
                        (ForwardStack a)
                        (AppTrain a)
                        ()
initForward state = do
  put state
  activation <- await
  yieldM $ maybe (throwError "No activation in stream to inputForward")
           (return . pure)
           activation

hiddenUnits :: [Unit a]
            -> ConduitM (ForwardStack a)
                        (ForwardStack a)
                        (AppTrain a)
                        ()
hiddenUnits [] =  mapM_C . const $
  (throwError "No units given to hiddenUnits")
hiddenUnits (unit:units) =
  unitForward unit .| hiddenUnits units

forwardPass :: [Unit a]
            -> CostFunction a
            -> TrainState a
            -> ConduitM (UnitActivation a)
                        (BackwardStack a)
                        (AppTrain a)
                        ()
forwardPass [] _ _ = mapM_C . const $
  (throwError "No units given to forwardPass")
forwardPass units cost state =
    initForward state
 .| hiddenUnits units
 .| applyCost cost
