{-# LANGUAGE NamedFieldPuns #-}

module School.Train.AppTrain
( AppTrain
, getParams
, putCost
, putCostParams
, putParamDerivs
, runTrainConduit
) where

import Conduit (ConduitM, runConduit)
import Control.Monad.State.Lazy (StateT, get, runStateT, put)
import Control.Monad.Except (Except, runExcept)
import Data.Void (Void)
import School.Train.TrainState (TrainState(..), CostParams)
import School.Types.PingPong (getPingPong)
import School.Unit.UnitParams (UnitParams)

type AppTrain a =
  StateT (TrainState a)
         (Except String)

getParams :: AppTrain a (UnitParams a)
getParams = do
  state@TrainState{ paramList } <- get
  let (p, pList) = getPingPong paramList
  put state { paramList = pList }
  return p

putParamDerivs :: UnitParams a -> AppTrain a ()
putParamDerivs derivs = do
  state@TrainState{ paramDerivs } <- get
  put state { paramDerivs = derivs:paramDerivs }

putCost :: a -> AppTrain a ()
putCost value = do
  state <- get
  put state { cost = value }

putCostParams :: CostParams -> AppTrain a ()
putCostParams params = do
  state <- get
  put state { costParams = params }

runTrainConduit :: ConduitM ()
                            Void
                            (AppTrain a)
                            b
                -> TrainState a
                -> Either String (b, TrainState a)
runTrainConduit conduit state = runExcept $
  runStateT (runConduit conduit) state

