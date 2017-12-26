{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.MultiNoulli
( multiNoulliTest ) where

import Conduit ((.|), await, liftIO, yield)
import Data.Default.Class (def)
import Numeric.LinearAlgebra ((><))
import School.TestUtils (addClasses, assertRight, diffCost, randomMatrix,
                         randomNNInts, singleClassOutput, testState, unitCorrect)
import School.Train.AppTrain (AppTrain)
import School.Train.ForwardPass (forwardPass)
import School.Types.FloatEq ((~=))
import School.Types.Slinky (Slinky(..), slinkySingleton)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.CostParams (CostParams(..))
import School.Unit.LogSoftMax (logSoftMax)
import School.Unit.MultiNoulli
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Utils.LinearAlgebra (compareDoubleMatrix)
import School.Utils.Tuple (trd3)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (monadicIO, assert, pre)

eps :: Double
eps = 1e-5

prec :: Double
prec = 5e-2

multi :: CostFunction Double (AppTrain a)
multi = multiNoulli Nothing Nothing

prop_correct_classes :: (Positive Int) -> (Positive Int) -> Property
prop_correct_classes (Positive c) (Positive b) = monadicIO $ do
  classes <- liftIO $ randomNNInts (c - 1) b
  let out = singleClassOutput c classes
  let params = slinkySingleton $ BatchClassTarget classes
  let cost = computeCost multi out params
  assert $ cost ~= Right (-1)

prop_wrong_classes :: (Positive Int) -> (Positive Int) -> Property
prop_wrong_classes (Positive c) (Positive b) = monadicIO $ do
  pre $ c > 1
  classes <- liftIO $ randomNNInts (c - 1) b
  let wrong = map (\k -> mod (k + 1) c) classes
  let out = singleClassOutput c wrong
  let params = slinkySingleton $ BatchClassTarget classes
  let cost = computeCost multi out params
  assert $ cost == Right 0

prop_norm :: (Positive Int) -> (Positive Int) -> Property
prop_norm (Positive c) (Positive b) = monadicIO $ do
  classes <- liftIO $ randomNNInts (c - 1) b
  input <- liftIO $ BatchActivation <$> randomMatrix b c
  let out = apply logSoftMax EmptyParams input
  let params = slinkySingleton $ BatchClassTarget classes
  let cost = computeCost multi out params
  assertRight (>= (-1)) cost

prop_numerical_deriv :: (Positive Int) -> (Positive Int) -> Property
prop_numerical_deriv (Positive c) (Positive b) = monadicIO $ do
  pre $ b < 23 && c < 23
  classes <- liftIO $ slinkySingleton . BatchClassTarget <$> randomNNInts (c - 1) b
  input <- liftIO $ BatchActivation <$> randomMatrix b c
  let deriv = derivCost multi input classes
  let check = (b >< c) [ diffCost multi
                                  input
                                  eps
                                  (j, k)
                                  classes
                               | j <- [0..b-1], k <- [0..c-1] ]
  assertRight (\(BatchGradient g) -> compareDoubleMatrix prec g check)
              deriv

prop_correct_forward_pass :: (Positive Int) -> (Positive Int) -> Property
prop_correct_forward_pass (Positive c) (Positive b) = monadicIO $ do
  classes <- liftIO $ randomNNInts (c - 1) b
  let forward = forwardPass [unitCorrect classes] multi
  activation <- liftIO $ randomMatrix b c
  let input = ([BatchActivation $ addClasses classes activation], SNil)
  let pass = yield input
          .| forward
          .| await
  result <- testState pass def
  assertRight ((maybe False ((~= (-1)) . trd3)) . fst)
              result

multiNoulliTest :: TestTree
multiNoulliTest = $(testGroupGenerator)
