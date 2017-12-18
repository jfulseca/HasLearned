{-# LANGUAGE NamedFieldPuns #-}

module School.Train.StateFunctions
( getParams
, putCost
, putCostParams
, putParamDerivs
) where

import Control.Monad.State.Lazy (get, put)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (TrainState(..))
import School.Types.PingPong (getPingPong)
import School.Types.Slinky (slinkyPrepend)
import School.Unit.CostParams (CostParams)
import School.Unit.UnitParams (UnitParams)

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
  state@TrainState { costParams } <- get
  let newParams = slinkyPrepend params costParams
  put state { costParams = newParams }

