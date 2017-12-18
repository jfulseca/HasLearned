module School.Train.BackwardPass
( backwardPass ) where

import Conduit ((.|), ConduitM,  mapC)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import School.Train.AppTrain (AppTrain)
import School.Unit.Unit (Unit)
import School.Unit.UnitBackward (BackwardStack, unitBackward)

backwardPass :: [Unit a]
             -> ConduitM (BackwardStack a)
                         (BackwardStack a)
                         (AppTrain a)
                         ()
backwardPass [] = lift . throwError $ "No units given to backwardPass"
backwardPass unitList = go unitList
  where go [] = mapC id
        go [unit] = unitBackward unit
        go (unit:units) = go units
                       .| unitBackward unit
