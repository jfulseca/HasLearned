module School.Train.BackwardPass
( backwardPass ) where

import Conduit ((.|), ConduitM,  mapC)
import School.App.AppS (AppS, throwConduit)
import School.Unit.Unit (Unit)
import School.Unit.UnitBackward (BackwardStack, unitBackward)

backwardPass :: [Unit a]
             -> ConduitM (BackwardStack a)
                         (BackwardStack a)
                         (AppS a)
                         ()
backwardPass [] = throwConduit "No units given to backwardPass"
backwardPass unitList = go unitList
  where go [] = mapC id
        go [unit] = unitBackward unit
        go (unit:units) = go units
                       .| unitBackward unit
