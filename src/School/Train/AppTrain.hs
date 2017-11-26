{-# LANGUAGE NamedFieldPuns #-}

module School.Train.AppTrain
( AppTrain
, getParams
, putCost
, putParamDerivs
, runTrainConduit
) where

import Conduit (ConduitM, runConduit)
import Control.Monad.State.Lazy (StateT, get, runStateT, put)
import Control.Monad.Except (Except, runExcept)
import Data.Void (Void)
import School.Train.TrainState (TrainState(..))
import School.Unit.UnitParams (UnitParams)

type AppTrain a =
  StateT (TrainState a)
         (Except String)

getParams :: AppTrain a (UnitParams a)
getParams = do
  state@TrainState{ paramList } <- get
  put state { paramList = tail paramList }
  return $ head paramList

putParamDerivs :: UnitParams a -> AppTrain a ()
putParamDerivs derivs = do
  state@TrainState{ paramDerivs } <- get
  put state { paramDerivs = derivs:paramDerivs }

putCost :: a -> AppTrain a ()
putCost value = do
  state <- get
  put state { cost = Just value }

runTrainConduit :: ConduitM ()
                            Void
                            (AppTrain a)
                            b
                -> TrainState a
                -> Either String (b, TrainState a)
runTrainConduit conduit state = runExcept $
  runStateT (runConduit conduit) state

