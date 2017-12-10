{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TypeSynonymInstances #-}

module School.Train.StoppingCondition
( StoppingCondition(..)
, maxIterations
) where

import School.Train.TrainState (TrainState(..))

newtype StoppingCondition a =
  StoppingCondition { runCondition :: TrainState a -> Bool }

instance Monoid (StoppingCondition a) where
  mappend c1 c2 =
    StoppingCondition { runCondition = run }
      where run s = (runCondition c1) s
                 || (runCondition c2) s
  mempty = StoppingCondition { runCondition }
    where runCondition = const False

maxIterations :: Int -> StoppingCondition a
maxIterations n =
  let runCondition = \(TrainState { iterationCount }) -> iterationCount >= n
  in StoppingCondition { runCondition }
