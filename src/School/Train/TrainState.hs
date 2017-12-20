{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module School.Train.TrainState
( HandlerStore(..)
, TrainState(..)
, def
) where

import Data.Default.Class (Default(..))
import Numeric.LinearAlgebra (Container, Vector)
import School.Types.PingPong (PingPong, pingPongSingleton)
import School.Unit.UnitParams (UnitParams(..))

data HandlerStore a = CostList [a]
                    | NoStore deriving (Show)

data TrainState a = TrainState
  { cost :: a
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

instance (Num a) => Default (TrainState a) where
  def = TrainState { cost = 0
                   , handlerStore = NoStore
                   , iterationCount = 0
                   , learningRate = 0
                   , paramDerivs = []
                   , paramList = pingPongSingleton EmptyParams
                   }
