module School.Train.ForwardPass
( forwardPass ) where

import Conduit ((.|), ConduitM, mapM_C)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain)
import School.Unit.ApplyCost (applyCost)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.Unit (Unit)
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
            -> CostFunction a (AppTrain a)
            -> ConduitM (ForwardStack a)
                        (BackwardStack a)
                        (AppTrain a)
                        ()
forwardPass [] _ = mapM_C . const $
  throwError "No units given to forwardPass"
forwardPass units cost =
    prepareCost cost
 .| hiddenUnits units
 .| applyCost cost
