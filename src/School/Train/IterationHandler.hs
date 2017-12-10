{-# LANGUAGE NamedFieldPuns #-}

module School.Train.IterationHandler
( IterationHandler(..)
, logCost
, storeCost
) where

import Conduit ((.|), ConduitM, liftIO, mapC, mapMC)
import Control.Monad.State.Lazy (get, put)
import School.App.AppS (AppS)
import School.Train.TrainState (HandlerStore(..), TrainState(..))
import System.FilePath (FilePath)

newtype IterationHandler a b =
  IterationHandler { runHandler :: ConduitM b b (AppS a) () }

toHandler :: ConduitM b b (AppS a) ()
          -> IterationHandler a b
toHandler runHandler = IterationHandler { runHandler }

instance Monoid (IterationHandler a b) where
  mappend (IterationHandler { runHandler = r1 })
          (IterationHandler { runHandler = r2 }) =
    toHandler $ r1 .| r2
  mempty = toHandler $ mapC id

storeCost :: IterationHandler a b
storeCost = toHandler . mapMC $ \input -> do
  state@TrainState { cost, handlerStore } <- get
  case handlerStore of
    CostList costs -> do
      let newStore = CostList (cost:costs)
      put state { handlerStore = newStore }
      return input
    _ -> return input

logCost :: (Show a)
        => FilePath
        -> IterationHandler a b
logCost path = toHandler $ do
  state <- get
  let str = (show . cost $ state) ++ "\n"
  liftIO $ appendFile path str
  mapC id
