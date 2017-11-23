{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.UnitForward
( unitForwardTest) where

import Conduit ((.|), ConduitM, runConduit, sinkList, yield)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (runStateT)
import Data.Either (isLeft)
import Numeric.LinearAlgebra (R)
import Data.Void (Void)
import School.TestUtils (randomAffineParams, randomMatrix)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (TrainState(..))
import School.Types.FloatEq ((~=))
import School.Types.PingPong (toPingPong)
import School.Unit.Affine (affine)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..), isApplyFail)
import School.Unit.UnitForward
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

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
  in isLeft result

prop_affine_param_fail :: Positive Int -> Positive Int -> Property
prop_affine_param_fail (Positive fSize) (Positive oSize) = monadicIO $ do
  let forward = unitForward affine
  actMat <- liftIO $ randomMatrix oSize fSize
  let acts = [BatchActivation actMat]
  let network =  yield acts
              .| forward
              .| sinkList
  let paramList = toPingPong [EmptyParams]
  let initState = TrainState { paramList }
  let result = runTest network initState
  either (const . assert $ False)
         (\(stack, _) -> assert $ isApplyFail . head . head $ stack)
         result

prop_affine_apply_single :: Positive Int ->  Positive Int -> Positive Int -> Property
prop_affine_apply_single (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  let forward = unitForward affine
  actMat <- liftIO $ randomMatrix bSize fSize
  let acts = [BatchActivation actMat]
  let network =  yield acts
              .| forward
              .| sinkList
  params <- liftIO $ randomAffineParams fSize oSize
  let paramList = toPingPong [params]
  let initState = TrainState { paramList }
  let result = runTest network initState
  let check = apply affine params (head acts)
  either (const . assert $ False)
         (\(stack, _) -> assert $ (head . head $ stack) ~= check)
         result

unitForwardTest :: TestTree
unitForwardTest = $(testGroupGenerator)
