module School.Train.TrainState
( emptyTrainState
, TrainState(..)
) where

import School.Types.PingPong (PingPong, toPingPong)
import School.Unit.UnitParams (UnitParams)

data TrainState a = TrainState
  { cost :: Maybe a
  , learningRate :: Maybe a
  , paramDerivs :: [UnitParams a]
  , paramList :: PingPong (UnitParams a)
  }

emptyTrainState :: TrainState a
emptyTrainState =
  TrainState { cost = Nothing
             , learningRate = Nothing
             , paramDerivs = []
             , paramList = toPingPong []
             }
