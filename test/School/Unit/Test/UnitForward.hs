{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.UnitForward
( unitForwardTest) where

import Conduit ((.|), sinkList, yield)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import School.TestUtils (randomAffineParams, randomMatrix, runTrainConduit)
import School.Train.TrainState (TrainState(..), emptyTrainState)
import School.Types.FloatEq ((~=))
import School.Types.PingPong (toPingPong)
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..), isApplyFail)
import School.Unit.UnitForward
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

prop_affine_input_fail :: Bool
prop_affine_input_fail = let
  forward = unitForward affine
  acts = [ApplyFail "init"]
  network =  yield acts
          .| forward
          .| sinkList
  result = runTrainConduit network emptyTrainState
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
  let initState = emptyTrainState { paramList }
  let result = runTrainConduit network initState
  either (const . assert $ False)
         (\(stack, _) -> assert $ isApplyFail . head . head $ stack)
         result

prop_affine_apply_single :: Positive Int
                         -> Positive Int
                         -> Positive Int
                         -> Property
prop_affine_apply_single (Positive bSize)
                         (Positive fSize)
                         (Positive oSize) = monadicIO $ do
  let forward = unitForward affine
  actMat <- liftIO $ randomMatrix bSize fSize
  let acts = [BatchActivation actMat]
  let network =  yield acts
              .| forward
              .| sinkList
  params <- liftIO $ randomAffineParams fSize oSize
  let paramList = toPingPong [params]
  let initState = emptyTrainState { paramList }
  let result = runTrainConduit network initState
  let check = apply affine params (head acts)
  either (const . assert $ False)
         (\(stack, _) -> assert $ (head . head $ stack) ~= check)
         result

prop_affine_apply_aff_rl_aff_rl :: Positive Int
                                -> Positive Int
                                -> Positive Int
                                -> Positive Int
                                -> Property
prop_affine_apply_aff_rl_aff_rl (Positive b)
                                (Positive f)
                                (Positive h)
                                (Positive o) = monadicIO $ do
  input <- liftIO $ randomMatrix b f
  let acts = [BatchActivation input]
  let network =  yield acts
              .| unitForward affine
              .| unitForward recLin
              .| unitForward affine
              .| unitForward recLin
              .| sinkList
  params1 <- liftIO $ randomAffineParams f h
  params2 <- liftIO $ randomAffineParams h o
  let paramList = toPingPong [ params1
                             , EmptyParams
                             , params2
                             , EmptyParams
                             ]
  let initState = emptyTrainState { paramList }
  let result = runTrainConduit network initState
  let out1 = apply affine params1 (head acts)
  let out2 = apply recLin EmptyParams out1
  let out3 = apply affine params2 out2
  let check = apply recLin EmptyParams out3
  either (const . assert $ False)
         (\(stack, _) -> assert $ (head . head $ stack) ~= check)
         result

unitForwardTest :: TestTree
unitForwardTest = $(testGroupGenerator)
