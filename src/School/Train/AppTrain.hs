{-# LANGUAGE NamedFieldPuns #-}

module School.Train.AppTrain
( AppTrain
, getParams
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
  TrainState{ paramDerivs, paramList } <- get
  put TrainState { paramDerivs
                 , paramList = tail paramList
                 }
  return $ head paramList

putParamDerivs :: UnitParams a -> AppTrain a ()
putParamDerivs derivs = do
  TrainState{ paramDerivs, paramList } <- get
  put TrainState { paramDerivs = derivs:paramDerivs
                 , paramList
                 }
