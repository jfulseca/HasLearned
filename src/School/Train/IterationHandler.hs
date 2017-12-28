{-# LANGUAGE NamedFieldPuns #-}

module School.Train.IterationHandler
( BackwardConduit
, IterationHandler(..)
, logCost
, storeCost
) where

import Conduit ((.|), ConduitM, liftIO, mapC, mapMC)
import Control.Monad.State.Lazy (get, put)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (HandlerStore(..), TrainState(..))
import School.Unit.UnitBackward (BackwardStack)
import System.FilePath (FilePath)

type BackwardConduit a = ConduitM (BackwardStack a)
                                  (BackwardStack a)
                                  (AppTrain a)
                                  ()

newtype IterationHandler a =
  IterationHandler { runHandler :: BackwardConduit a }

toHandler :: BackwardConduit a
          -> IterationHandler a
toHandler runHandler = IterationHandler { runHandler }

instance Monoid (IterationHandler a) where
  mappend IterationHandler { runHandler = r1 }
          IterationHandler { runHandler = r2 } =
    toHandler $ r1 .| r2
  mempty = toHandler $ mapC id

storeCost :: IterationHandler a
storeCost = toHandler . mapMC $ \(stack, grad, cost) -> do
  state@TrainState { handlerStore } <- get
  case handlerStore of
    CostList costs -> do
      let newStore = CostList (cost:costs)
      put state { handlerStore = newStore }
      return (stack, grad, cost)
    _ -> return (stack, grad, cost)

logCost :: (Show a)
        => FilePath
        -> IterationHandler a
logCost path = toHandler . mapMC $ \(stack, grad, cost) -> do
  let str = show cost ++ "\n"
  liftIO $ appendFile path str
  return (stack, grad, cost)
