{-# LANGUAGE NamedFieldPuns #-}

module School.Train.IterationHandler
( IterationHandler
, storeCost
) where

import Conduit (ConduitM, mapC)
import Control.Monad.State.Lazy (get, put)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (HandlerStore(..), TrainState(..))

type IterationHandler a =
  ConduitM () () (AppTrain a) ()

storeCost :: IterationHandler a
storeCost = do
  state@TrainState { cost, handlerStore = (CostList costs) } <- get
  let newStore = CostList (cost:costs)
  put state { handlerStore = newStore }
  mapC id
