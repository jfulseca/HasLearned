module School.Train.TrainState
( TrainState(..) ) where

import School.Types.PingPong (PingPong)
import School.Unit.UnitParams (UnitParams)

data TrainState a = TrainState
  { paramList :: PingPong (UnitParams a)
  }
