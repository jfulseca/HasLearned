module School.Train.UpdateParams
( UpdateParams ) where

import School.Train.TrainState (TrainState)

type UpdateParams a = TrainState a
                   -> TrainState a
