{-# LANGUAGE NamedFieldPuns #-}

module School.Train.IterationHandler
( IterationHandler
, noHandling
, logCost
, storeCost
) where

import Conduit (ConduitM, liftIO, mapC, mapMC)
import Control.Monad.State.Lazy (get, put)
import School.App.AppS (AppS)
import School.Train.TrainState (HandlerStore(..), TrainState(..))
import System.FilePath (FilePath)

type IterationHandler a b =
  ConduitM b b (AppS a) ()

storeCost :: IterationHandler a b
storeCost = mapMC $ \input -> do
  state@TrainState { cost, handlerStore } <- get
  case handlerStore of
    CostList costs -> do
      let newStore = CostList (cost:costs)
      put state { handlerStore = newStore }
      return input
    _ -> return input

noHandling :: IterationHandler a b
noHandling = mapC id

logCost :: (Show a)
        => FilePath
        -> IterationHandler a b
logCost path = do
  state <- get
  let str = (show . cost $ state) ++ "\n"
  liftIO $ appendFile path str
  mapC id
