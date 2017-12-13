{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.UnitForward
( unitForwardTest) where

import Conduit ((.|), sinkList, yield)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import School.TestUtils (assertRight, fromRight, randomAffineParams, randomMatrix, testState)
import School.Train.TrainState (TrainState(..), def)
import School.Types.FloatEq ((~=))
import School.Types.PingPong (pingPongSingleton, toPingPong)
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

prop_affine_input_fail :: Property
prop_affine_input_fail = monadicIO $ do
  let forward = unitForward affine
  let acts = [ApplyFail "init"]
  let network =  yield acts
              .| forward
              .| sinkList
  result <- testState network def
  assert $ isLeft result

prop_affine_param_fail :: Positive Int -> Positive Int -> Property
prop_affine_param_fail (Positive fSize) (Positive oSize) = monadicIO $ do
  let forward = unitForward affine
  actMat <- liftIO $ randomMatrix oSize fSize
  let acts = [BatchActivation actMat]
  let network =  yield acts
              .| forward
              .| sinkList
  result <- testState network def
  assertRight (isApplyFail . head . head . fst)
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
  let paramList = pingPongSingleton params
  let initState = def { paramList }
  result <- testState network initState
  let check = apply affine params (head acts)
  assertRight (((~=) check) . head . head . fst)
              result

prop_apply_aff_rl_aff_rl :: Positive Int
                         -> Positive Int
                         -> Positive Int
                         -> Positive Int
                         -> Property
prop_apply_aff_rl_aff_rl (Positive b)
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
  let allParams = toPingPong [ params1
                             , EmptyParams
                             , params2
                             , EmptyParams
                             ]
  let paramList = fromRight (pingPongSingleton EmptyParams) allParams
  let initState = def { paramList }
  result <- testState network initState
  let out1 = apply affine params1 (head acts)
  let out2 = apply recLin EmptyParams out1
  let out3 = apply affine params2 out2
  let check = apply recLin EmptyParams out3
  assertRight (((~=) check) . head . head . fst)
              result

unitForwardTest :: TestTree
unitForwardTest = $(testGroupGenerator)
