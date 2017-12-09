{-# LANGUAGE NamedFieldPuns #-}

module School.Train.StoppingCondition
( StoppingCondition
, maxIterations
) where

import School.Train.TrainState (TrainState(..))

type StoppingCondition a = TrainState a -> Bool

maxIterations :: Int -> StoppingCondition a
maxIterations n (TrainState { iterationCount }) =
  iterationCount >= n 
