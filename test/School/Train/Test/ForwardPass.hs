{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.ForwardPass
( forwardPassTest ) where

import Conduit ((.|), yield, liftIO, sinkList, await)
import Data.Either (isLeft)
import Numeric.LinearAlgebra ((><), Element, Matrix, R, ident)
import School.Types.PingPong (pingPongSingleton, reversePingPong, toPingPong)
import School.Unit.Affine (affine)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.RecLin (recLin)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.TestUtils (fromRight, randomAffineParams, randomMatrix)
import School.Train.AppTrain (runTrainConduit)
import School.Train.ForwardPass
import School.Train.TrainState (TrainState(..), emptyTrainState)
import School.Unit.WeightDecay (weightDecay)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

weight1 :: CostFunction R
weight1 = weightDecay 1

prop_no_units :: Bool
prop_no_units = let
  forward = forwardPass [] weight1
  source = yield . BatchActivation $ ident 1
  pass = source .| forward .| sinkList
  result = runTrainConduit pass emptyTrainState
  in isLeft result

empty :: (Element a) => Matrix a
empty = (0><0) []

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

prop_single_recLin :: (Positive Int) -> (Positive Int) -> Property
prop_single_recLin (Positive bSize) (Positive fSize) = monadicIO $ do
  let forward = forwardPass [recLin] weight1
  input <- liftIO $ BatchActivation <$> (randomMatrix bSize fSize)
  let source = yield input
  let pass = source .| forward .| await
  let result = runTrainConduit pass emptyTrainState
  let out = apply recLin EmptyParams input
  let (cost, grad) = doCost weight1 out
  let bParams = reversePingPong . paramList $ emptyTrainState
  let state = emptyTrainState { cost, paramList = bParams }
  let stack = ([input], grad)
  let check = Right $ (Just stack, state) :: Either String (Maybe (BackwardStack R), TrainState R)
  assert $ result == check

prop_aff_rl_aff_rl :: (Positive Int) -> (Positive Int) -> (Positive Int) -> (Positive Int) -> Property
prop_aff_rl_aff_rl (Positive b) (Positive f) (Positive h) (Positive o) = monadicIO $ do
  let units = [affine, recLin, affine, recLin]
  let forward = forwardPass units weight1
  input <- liftIO $ BatchActivation <$> (randomMatrix b f)
  let source = yield input
  let pass = source .| forward .| await
  params1 <- liftIO $ randomAffineParams f h
  params2 <- liftIO $ randomAffineParams h o
  let allParams = toPingPong [ params1
                             , EmptyParams
                             , params2
                             , EmptyParams
                             ]
  let paramList = fromRight (pingPongSingleton EmptyParams) allParams
  let initState = emptyTrainState { paramList }
  let result = runTrainConduit pass initState
  let out1 = apply affine params1 input
  let out2 = apply recLin EmptyParams out1
  let out3 = apply affine params2 out2
  let out4 = apply recLin EmptyParams out3
  let (cost, grad) = doCost weight1 out4
  let bParams = reversePingPong paramList
  let state = emptyTrainState { cost, paramList = bParams }
  let stack = ([out3, out2, out1, input], grad)
  let check = Right $ (Just stack, state) :: Either String (Maybe (BackwardStack R), TrainState R)
  assert $ result == check

forwardPassTest:: TestTree
forwardPassTest = $(testGroupGenerator)
