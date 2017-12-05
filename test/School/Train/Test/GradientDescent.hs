{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.GradientDescent
( gradientDescentTest ) where

import Conduit (liftIO, mapC, yield, yieldMany)
import Data.Either (isLeft)
import School.TestUtils (empty, isSorted, randomAffineParams,
                         randomMatrix, weight1)
import School.Train.GradientDescent
import School.Train.SimpleDescentUpdate (simpleDescentUpdate)
import School.Train.StoppingCondition (StoppingCondition)
import School.Train.IterationHandler (storeCost, noHandling)
import School.Train.TrainState (TrainState(..), HandlerStore(..), emptyTrainState)
import School.Types.PingPong (pingPongSingleton)
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.UnitActivation (UnitActivation(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)


prop_no_units :: Bool
prop_no_units = let
  result = gradientDescent (yield . BatchActivation $ empty)
                           []
                           weight1
                           simpleDescentUpdate
                           (const False)
                           (mapC id)
                           emptyTrainState
  in isLeft result

itStop :: Int -> StoppingCondition Double
itStop maxIt (TrainState { iterationCount }) =
  iterationCount >= maxIt

prop_iterations :: Positive Int -> Positive Int -> Positive Int -> Property
prop_iterations (Positive n) (Positive b) (Positive f) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let source = yieldMany . repeat $ input
  let result = gradientDescent source
                               [recLin]
                               weight1
                               simpleDescentUpdate
                               (itStop n)
                               noHandling
                               emptyTrainState
  either (const $ assert False)
         (\TrainState { iterationCount } ->
            assert $ iterationCount == n)
         result

prop_cost_decline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_cost_decline (Positive b) (Positive f) (Positive o) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrix b f
  let source = yieldMany . repeat $ input
  paramList <- liftIO $ pingPongSingleton <$> randomAffineParams f o
  let handlerStore = CostList []
  let initState = emptyTrainState { handlerStore
                                  , learningRate = 1e-2
                                  , paramList
                                  }
  let result = gradientDescent source
                               [affine]
                               weight1
                               simpleDescentUpdate
                               (itStop 5)
                               storeCost
                               initState
  either (const $ assert False)
         (\TrainState { handlerStore = (CostList costs) } ->
            assert $ isSorted costs)
         result

gradientDescentTest :: TestTree
gradientDescentTest = $(testGroupGenerator)
