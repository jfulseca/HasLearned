{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.BackwardPass
( backwardPassTest ) where

import Conduit ((.|), yield, liftIO, sinkList, await)
import Data.Either (isLeft)
import Numeric.LinearAlgebra ((><), Element, Matrix, R)
import School.TestUtils (fromRight, randomAffineParams, randomMatrix)
import School.Train.AppTrain (runTrainConduit)
import School.Train.BackwardPass
import School.Train.TrainState (TrainState(..), emptyTrainState)
import School.Types.PingPong (pingPongSingleton, reversePingPong, toPingPong)
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Unit.WeightDecay (weightDecay)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

empty :: (Element a) => Matrix a
empty = (0><0) []

weight1 :: CostFunction R
weight1 = weightDecay 1

doCost :: (Element a, Num a)
       => CostFunction a
       -> UnitActivation a
       -> (a, UnitGradient a)
doCost costFunction activation =
  fromRight (0, BatchGradient empty) result where
    result = do
      cost <- computeCost costFunction activation
      grad <- derivCost costFunction activation
      return (cost, grad)

prop_no_units :: Bool
prop_no_units = let
  backward = backwardPass []
  source = yield (([BatchActivation empty], BatchGradient empty) :: BackwardStack R)
  pass = source .| backward .| sinkList
  result = runTrainConduit pass emptyTrainState
  in isLeft result

prop_single_affine :: (Positive Int) -> (Positive Int) -> (Positive Int) -> Property
prop_single_affine (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  let backward = backwardPass [affine]
  input <- liftIO $ BatchActivation <$> (randomMatrix bSize fSize)
  grad <- liftIO $ BatchGradient <$> (randomMatrix bSize oSize)
  params <- liftIO $ randomAffineParams fSize bSize
  let source = yield ([input], grad)
  let pass = source .| backward .| await
  let pl = reversePingPong $ pingPongSingleton params
  let state = emptyTrainState { paramList = pl }
  let result = runTrainConduit pass state
  let (_, check) = deriv affine params grad input
  either (const $ assert False)
         (\(_, TrainState { paramDerivs }) ->
            assert $ paramDerivs == [check])
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
  let (cost, grad) = doCost weight1 out4
  let (grad2, deriv1) = deriv recLin EmptyParams grad out3
  let (grad3, deriv2) = deriv affine params2 grad2 out2
  let (grad4, deriv3) = deriv recLin EmptyParams grad3 out1
  let (_, deriv4) = deriv affine params1 grad4 input
  let source = yield ([out3, out2, out1, input], grad)
  let pass = source .| backward .| await
  let initState = emptyTrainState { cost, paramList }
  let result = runTrainConduit pass initState
  let check = [deriv4, deriv3, deriv2, deriv1]
  either (const $ assert False)
         (\(_, TrainState { paramDerivs }) ->
            assert $ check == paramDerivs)
         result

backwardPassTest :: TestTree
backwardPassTest = $(testGroupGenerator)

