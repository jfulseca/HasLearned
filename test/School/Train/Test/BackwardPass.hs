{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.BackwardPass
( backwardPassTest ) where

import Conduit ((.|), Identity, yield, liftIO, sinkList, await)
import Data.Either (isLeft)
import School.TestUtils (assertRight, empty, doCost, fromRight, randomAffineParams,
                         randomMatrix, testState, weight1)
import School.Train.BackwardPass
import School.Train.TrainState (TrainState(..), def)
import School.Types.PingPong (pingPongSingleton, reversePingPong, toPingPong)
import School.Types.Slinky (Slinky(..))
import School.Unit.Affine (affine)
import School.Unit.CostFunction (CostFunction)
import School.Unit.RecLin (recLin)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

prop_no_units :: Property
prop_no_units = monadicIO $ do
  let backward = backwardPass []
  let source = yield (([BatchActivation empty],
                       BatchGradient empty,
                       0 ) :: BackwardStack Double)
  let pass = source .| backward .| sinkList
  result <- testState pass def
  assert $ isLeft result

prop_single_affine :: (Positive Int) -> (Positive Int) -> (Positive Int) -> Property
prop_single_affine (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  let backward = backwardPass [affine]
  input <- liftIO $ BatchActivation <$> (randomMatrix bSize fSize)
  grad <- liftIO $ BatchGradient <$> (randomMatrix bSize oSize)
  params <- liftIO $ randomAffineParams fSize bSize
  let source = yield ([input], grad, 0)
  let pass = source .| backward .| await
  let pl = reversePingPong $ pingPongSingleton params
  let state = def { paramList = pl }
  result <- testState pass state
  let (_, check) = deriv affine params grad input
  assertRight ((== [check]) . paramDerivs . snd)
              result

prop_aff_rl_aff_rl :: (Positive Int) -> (Positive Int) -> (Positive Int) -> (Positive Int) -> Property
prop_aff_rl_aff_rl (Positive b) (Positive f) (Positive h) (Positive o) = monadicIO $ do
  let units = [affine, recLin, affine, recLin]
  let backward = backwardPass units
  input <- liftIO $ BatchActivation <$> (randomMatrix b f)
  params1 <- liftIO $ randomAffineParams f h
  params2 <- liftIO $ randomAffineParams h o
  let allParams = toPingPong [ params1
                             , EmptyParams
                             , params2
                             , EmptyParams
                             ]
  let paramList = reversePingPong $ fromRight (pingPongSingleton EmptyParams) allParams
  let out1 = apply affine params1 input
  let out2 = apply recLin EmptyParams out1
  let out3 = apply affine params2 out2
  let out4 = apply recLin EmptyParams out3
  let weight = weight1 :: CostFunction Double Identity
  let (cost, grad) = doCost weight out4 SNil
  let (grad2, deriv1) = deriv recLin EmptyParams grad out3
  let (grad3, deriv2) = deriv affine params2 grad2 out2
  let (grad4, deriv3) = deriv recLin EmptyParams grad3 out1
  let (_, deriv4) = deriv affine params1 grad4 input
  let source = yield ([out3, out2, out1, input], grad, cost)
  let pass = source .| backward .| await
  let initState = def { paramList }
  result <- testState pass initState
  let check = [deriv4, deriv3, deriv2, deriv1]
  assertRight ((== check) . paramDerivs . snd)
              result

backwardPassTest :: TestTree
backwardPassTest = $(testGroupGenerator)
