{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.GradientDescentPass
( gradientDescentPassTest ) where

import Conduit ((.|), yield, liftIO, await)
import Data.Either (isLeft)
import School.TestUtils (doCost, empty, fromRight, randomAffineParams, randomMatrix, weight1)
import School.Train.AppTrain (runTrainConduit)
import School.Train.GradientDescentPass
import School.Train.SimpleDescentUpdate (simpleDescentUpdate)
import School.Train.TrainState (TrainState(..), emptyTrainState)
import School.Types.PingPong (pingPongSingleton, toPingPong)
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

prop_no_units :: Bool
prop_no_units = let
  descent = gradientDescentPass [] weight1 simpleDescentUpdate
  pass = yield (BatchActivation empty)
      .| descent
      .| await
  result = runTrainConduit pass emptyTrainState
  in isLeft result

prop_single_reclin :: Positive Int -> Positive Int -> Property
prop_single_reclin (Positive b) (Positive f) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let descent = gradientDescentPass [recLin] weight1 simpleDescentUpdate
  let pass = yield input
          .| descent
          .| await
  let result = runTrainConduit pass emptyTrainState
  let out = apply recLin EmptyParams input
  let (cost, _) = doCost weight1 out
  let state = emptyTrainState { cost, paramDerivs = [EmptyParams] }
  let check = Right (Nothing, state) :: Either String (Maybe (), TrainState Double)
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
  let initState = emptyTrainState { paramList }
  let result = runTrainConduit pass initState
  let out1 = apply affine params input
  let out2 = apply recLin EmptyParams out1
  let (cost, grad1) = doCost weight1 out2
  let (grad2, deriv2) = deriv recLin EmptyParams grad1 out1
  let (_, deriv1) = deriv affine params grad2 input
  let paramDerivs = [deriv1, deriv2]
  let state = emptyTrainState { cost
                              , paramDerivs
                              , paramList
                              }
  let check = Right (Nothing, state) :: Either String (Maybe (), TrainState Double)
  assert $ result == check

gradientDescentPassTest :: TestTree
gradientDescentPassTest = $(testGroupGenerator)
