{-# LANGUAGE NamedFieldPuns #-}

module School.Train.AppTrain
( AppTrain
, getParams
) where

import Control.Monad.State.Lazy (StateT, get, put)
import Control.Monad.Except (Except)
import School.Train.TrainState (TrainState(..))
import School.Unit.UnitParams (UnitParams)

type AppTrain a =
  StateT (TrainState a)
         (Except String)

getParams :: AppTrain a (UnitParams a)
getParams = do
  TrainState{ paramList } <- get
  put . TrainState . tail $ paramList
  return $ head paramList
