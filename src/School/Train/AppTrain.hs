{-# LANGUAGE NamedFieldPuns #-}

module School.Train.AppTrain
( AppTrain
, getParams
, putCost
, putParamDerivs
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


