{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Train.TrainState
( CostParams(..)
, HandlerStore(..)
, TrainState(..)
, emptyTrainState
) where

import Numeric.LinearAlgebra (Container, Vector)
import School.Types.PingPong (PingPong, pingPongSingleton)
import School.Unit.UnitParams (UnitParams(..))

data HandlerStore a = CostList [a]
                    | NoStore deriving (Show)

data CostParams = BatchClassTarget [Int]
                | NoCostParams deriving (Show)

data TrainState a = TrainState
  { cost :: a
  , costParams :: CostParams
  , handlerStore :: HandlerStore a
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
             , costParams = NoCostParams
             , handlerStore = NoStore
             , iterationCount = 0
             , learningRate = 0
             , paramDerivs = []
             , paramList = pingPongSingleton EmptyParams
             }
