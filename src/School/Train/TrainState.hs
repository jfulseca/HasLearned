{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Train.TrainState
( emptyTrainState
, TrainState(..)
) where

import Numeric.LinearAlgebra (Container, Vector)
import School.Types.PingPong (PingPong, pingPongSingleton)
import School.Unit.UnitParams (UnitParams(..))

data TrainState a = TrainState
  { cost :: a
  , iterationCount :: Int
  , learningRate :: a
  , paramDerivs :: [UnitParams a]
  , paramList :: PingPong (UnitParams a)
  } deriving (Show)

instance (Container Vector a, Eq a, Num a) => Eq (TrainState a) where
  s1 == s2 = cost s1 == cost s2
          && iterationCount s1 == iterationCount s2
          && learningRate s1 == learningRate s2
          && paramDerivs s1 == paramDerivs s2
          && paramList s1 == paramList s2

emptyTrainState :: (Num a) => TrainState a
emptyTrainState =
  TrainState { cost = 0
             , iterationCount = 0
             , learningRate = 0
             , paramDerivs = []
             , paramList = pingPongSingleton EmptyParams
             }
