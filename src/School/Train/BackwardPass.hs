module School.Train.BackwardPass
( backwardPass ) where

import Conduit ((.|), ConduitM,  mapC, mapM_C, sinkNull)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain)
import School.Unit.Unit (Unit)
import School.Unit.UnitBackward (BackwardStack, unitBackward)

backwardPass :: [Unit a]
             -> ConduitM (BackwardStack a)
                         ()
                         (AppTrain a)
                         ()
backwardPass [] = mapM_C . const $
  (throwError "No units given to backwardPass")
backwardPass unitList = go unitList .| sinkNull
  where go [] = mapC id
        go [unit] = unitBackward unit
        go (unit:units) = go units
                       .| unitBackward unit
