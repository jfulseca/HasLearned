module School.Train.StoppingCondition
( StoppingCondition ) where

import School.Train.TrainState (TrainState)

type StoppingCondition a = TrainState a -> Bool
