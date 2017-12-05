module School.Train.ForwardPass
( forwardPass ) where

import Conduit ((.|), ConduitM, mapC, mapM_C)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain)
import School.Unit.ApplyCost (applyCost)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack, unitForward)

hiddenUnits :: [Unit a]
            -> ConduitM (ForwardStack a)
                        (ForwardStack a)
                        (AppTrain a)
                        ()
hiddenUnits [] = throwError "No units given to hiddenUnits"
hiddenUnits [unit] = unitForward unit
hiddenUnits (unit:units) =
  unitForward unit .| hiddenUnits units

forwardPass :: [Unit a]
            -> CostFunction a
            -> ConduitM (UnitActivation a)
                        (BackwardStack a)
                        (AppTrain a)
                        ()
forwardPass [] _ = mapM_C . const $
  (throwError "No units given to forwardPass")
forwardPass units cost =
    mapC pure
 .| hiddenUnits units
 .| applyCost cost
