{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TypeSynonymInstances #-}

module School.Train.StoppingCondition
( StoppingCondition(..)
, maxIterations
) where

import School.Train.TrainState (TrainState(..))

newtype StoppingCondition a =
  StoppingCondition { runCondition :: TrainState a -> Bool }

toCondition :: (TrainState a -> Bool)
            -> StoppingCondition a
toCondition runCondition =
  StoppingCondition { runCondition }

instance Monoid (StoppingCondition a) where
  mappend c1 c2 =
    toCondition $ \s -> (runCondition c1) s
                     || (runCondition c2) s
  mempty = toCondition $ const False

maxIterations :: Int -> StoppingCondition a
maxIterations n =
  toCondition $ \s -> iterationCount s >= n
