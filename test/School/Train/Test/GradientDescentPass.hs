{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.GradientDescentPass
( gradientDescentPassTest ) where

import Conduit ((.|), await, liftIO, sinkList, yield, yieldMany)
import Data.Either (isLeft)
import School.TestUtils (assertRight, doCost, empty, fromRight, randomAffineParams,
                         randomMatrix, testState, weight1)
import School.Train.GradientDescentPass
import School.Train.SimpleDescentUpdate (simpleDescentUpdate)
import School.Train.TrainState (TrainState(..), defTrainState)
import School.Types.PingPong (pingPongSingleton, toPingPong)
import School.Unit.Affine (affine)
import School.Unit.CostParams (LinkedParams(..))
import School.Unit.RecLin (recLin)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

prop_no_units :: Property
prop_no_units = monadicIO $ do
  let descent = gradientDescentPass [] weight1 simpleDescentUpdate
  let pass = yield (BatchActivation empty)
          .| descent
          .| await
  result <- testState pass defTrainState
  assert $ isLeft result

prop_single_reclin :: Positive Int -> Positive Int -> Property
prop_single_reclin (Positive b) (Positive f) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let descent = gradientDescentPass [recLin] weight1 simpleDescentUpdate
  let pass = yield input
          .| descent
          .| await
  result <- testState pass defTrainState
  let out = apply recLin EmptyParams input
  let (cost, grad) = doCost weight1 out NoNode
  let state = defTrainState { cost
                              , iterationCount = 1
                              }
  let check = Right (Just grad, state)
  assert $ result == check

prop_affine_reclin :: Positive Int -> Positive Int -> Positive Int -> Property
prop_affine_reclin (Positive b) (Positive f) (Positive o) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let units = [affine, recLin]
  params <- liftIO $ randomAffineParams f o
  let descent = gradientDescentPass units weight1 simpleDescentUpdate
  let pass = yield input
          .| descent
          .| await
  let paramList = fromRight (pingPongSingleton EmptyParams)
                            (toPingPong [params, EmptyParams])
  let learningRate = 1 :: Double
  let initState = defTrainState { learningRate, paramList }
  result <- testState pass initState
  let out1 = apply affine params input
  let out2 = apply recLin EmptyParams out1
  let (cost, grad1) = doCost weight1 out2 NoNode
  let (grad2, deriv2) = deriv recLin EmptyParams grad1 out1
  let (grad3, deriv1) = deriv affine params grad2 input
  let paramDerivs = [deriv1, deriv2]
  let state = defTrainState { cost
                              , iterationCount = 1
                              , learningRate
                              , paramDerivs
                              , paramList
                              } :: TrainState Double
  let newState = either (const defTrainState)
                        id
                        (simpleDescentUpdate state)
  let check = Right (Just grad3, newState { paramDerivs = [] })
  assert $ result == check

prop_stream_several :: Positive Int -> Positive Int -> Positive Int -> Property
prop_stream_several (Positive n) (Positive b) (Positive f) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let descent = gradientDescentPass [recLin] weight1 simpleDescentUpdate
  let source = yieldMany . (replicate n) $ input
  let pass = source
          .| descent
          .| sinkList
  result <- testState pass defTrainState
  assertRight ((\g -> length g == n) . fst) result

gradientDescentPassTest :: TestTree
gradientDescentPassTest = $(testGroupGenerator)
