{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.UnitForward
( unitForwardTest) where

import Conduit ((.|), ConduitM, runConduit, sinkList, yield)
import Control.Monad.Except (runExcept)
import Control.Monad.State.Lazy (runStateT)
import Data.Either (isLeft)
import Numeric.LinearAlgebra (R)
import Data.Void (Void)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (TrainState(..))
import School.Types.PingPong (toPingPong)
import School.Unit.Affine (affine)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH

runTest :: ConduitM ()
                    Void
                    (AppTrain R)
                    a
        -> TrainState Double
        -> Either String (a, TrainState R)
runTest conduit state = runExcept $
  runStateT (runConduit conduit) state

prop_affine_input_fail :: Bool
prop_affine_input_fail = let
  forward = unitForward affine
  acts = [ApplyFail "init"]
  network =  yield acts
          .| forward
          .| sinkList
  paramList = toPingPong [EmptyParams]
  initState = TrainState { paramList }
  result = runTest network initState
--  result =
--    runExcept $ runStateT (runConduit network) initState
  in isLeft result

unitForwardTest :: TestTree
unitForwardTest = $(testGroupGenerator)
