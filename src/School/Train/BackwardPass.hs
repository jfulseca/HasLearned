module School.Train.BackwardPass
( backwardPass ) where

import Conduit ((.|), ConduitM,  mapM_C)
import Control.Monad.Except (throwError)
import Data.Void (Void)
import School.Train.AppTrain (AppTrain)
import School.Unit.Unit (Unit)
import School.Unit.UnitBackward (BackwardStack, unitBackward)

backwardPass :: [Unit a]
             -> ConduitM (BackwardStack a)
                         Void
                         (AppTrain a)
                         ()
backwardPass [] = mapM_C . const $
  (throwError "No units given to backwardPass")
backwardPass (unit:units) =
  unitBackward unit .| backwardPass units
