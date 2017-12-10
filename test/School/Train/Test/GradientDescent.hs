{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.GradientDescent
( gradientDescentTest ) where

import Conduit (liftIO, mapC, yield, yieldMany)
import Data.Either (isLeft)
import School.TestUtils (assertRight, empty, isSorted,
                         randomAffineParams, randomMatrix, weight1)
import School.Train.GradientDescent
import School.Train.SimpleDescentUpdate (simpleDescentUpdate)
import School.Train.StoppingCondition (maxIterations)
import School.Train.IterationHandler (storeCost, noHandling)
import School.Train.TrainState (TrainState(..), HandlerStore(..), defTrainState)
import School.Types.PingPong (pingPongSingleton)
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.UnitActivation (UnitActivation(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)


prop_no_units :: Property
prop_no_units = monadicIO $ do
  result <- liftIO $ gradientDescent (yield . BatchActivation $ empty)
                                     []
                                     weight1
                                     simpleDescentUpdate
                                     mempty
                                     (mapC id)
                                     defTrainState
  assert $ isLeft result

prop_iterations :: Positive Int -> Positive Int -> Positive Int -> Property
prop_iterations (Positive n) (Positive b) (Positive f) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let source = yieldMany . repeat $ input
  result <- liftIO $ gradientDescent source
                                    [recLin]
                                    weight1
                                    simpleDescentUpdate
                                    (maxIterations n)
                                    noHandling
                                    defTrainState
  assertRight ((== n) . iterationCount) result

prop_cost_decline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_cost_decline (Positive b) (Positive f) (Positive o) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let source = yieldMany . repeat $ input
  paramList <- liftIO $ pingPongSingleton <$> randomAffineParams f o
  let store = CostList []
  let initState = defTrainState { handlerStore = store
                                  , learningRate = 1e-2
                                  , paramList
                                  }
  result <- liftIO $ gradientDescent source
                                     [affine]
                                     weight1
                                     simpleDescentUpdate
                                     (maxIterations 5)
                                     storeCost
                                     initState
  assertRight ((\(CostList c) -> isSorted c) . handlerStore)
              result

gradientDescentTest :: TestTree
gradientDescentTest = $(testGroupGenerator)
