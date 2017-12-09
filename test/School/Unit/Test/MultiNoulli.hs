{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.MultiNoulli
( multiNoulliTest ) where

import Conduit ((.|), await, yield)
import Control.Monad.IO.Class (liftIO)
import Numeric.LinearAlgebra ((><), Element, Matrix, R, Vector, assoc,
                              cols, fromColumns, fromRows, fromList, toColumns)
import School.TestUtils (assertRight, diffCost, randomMatrix, randomNNInts, testState)
import School.Train.ForwardPass (forwardPass)
import School.Train.TrainState (TrainState(..), defTrainState)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.CostParams (CostParams(..), paramSingleton)
import School.Unit.LogSoftMax (logSoftMax)
import School.Unit.MultiNoulli
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Utils.LinearAlgebra (compareDoubleMatrix)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (monadicIO, assert, pre)

eps :: Double
eps = 1e-5

prec :: Double
prec = 5e-2

appCol :: (Element a)
       => Vector a
       -> Matrix a
       -> Matrix a
appCol v =
  fromColumns . (flip (++) $ [v]) . toColumns

addClasses :: (Element a, Num a)
           => [Int]
           -> Matrix a
           -> Matrix a
addClasses classes mat = appCol v mat where
  v = fromList . (map fromIntegral) $ classes

singleClassOutput :: Int -> [Int] -> UnitActivation R
singleClassOutput nClasses =
  BatchActivation . fromRows . (map (\idx -> assoc nClasses 0 [(idx, 1)]))

prop_correct_classes :: (Positive Int) -> (Positive Int) -> Property
prop_correct_classes (Positive c) (Positive b) = monadicIO $ do
  classes <- liftIO $ randomNNInts (c - 1) b
  let out = singleClassOutput c classes
  let params = paramSingleton $ BatchClassTarget classes
  let cost = computeCost multiNoulli out params
  assert $ cost == Right (-1)

prop_wrong_classes :: (Positive Int) -> (Positive Int) -> Property
prop_wrong_classes (Positive c) (Positive b) = monadicIO $ do
  pre $ c > 1
  classes <- liftIO $ randomNNInts (c - 1) b
  let wrong = map (\k -> mod (k + 1) c) classes
  let out = singleClassOutput c wrong
  let params = paramSingleton $ BatchClassTarget classes
  let cost = computeCost multiNoulli out params
  assert $ cost == Right 0

prop_norm :: (Positive Int) -> (Positive Int) -> Property
prop_norm (Positive c) (Positive b) = monadicIO $ do
  classes <- liftIO $ randomNNInts (c - 1) b
  input <- liftIO $ BatchActivation <$> randomMatrix b c
  let out = apply logSoftMax EmptyParams input
  let params = paramSingleton $ BatchClassTarget classes
  let cost = computeCost multiNoulli out params
  assertRight (>= (-1)) cost

prop_numerical_deriv :: (Positive Int) -> (Positive Int) -> Property
prop_numerical_deriv (Positive c) (Positive b) = monadicIO $ do
  pre $ b < 23 && c < 23
  classes <- liftIO $ paramSingleton . BatchClassTarget <$> randomNNInts (c - 1) b
  input <- liftIO $ BatchActivation <$> randomMatrix b c
  let deriv = derivCost multiNoulli input classes
  let check = (b >< c) [ diffCost multiNoulli
                                  input
                                  eps
                                  (j, k)
                                  classes
                               | j <- [0..b-1], k <- [0..c-1] ]
  assertRight (\(BatchGradient g) -> compareDoubleMatrix prec g check)
              deriv

unitCorrect :: [Int] -> Unit R
unitCorrect classes = Unit { apply, deriv } where
  apply _ (BatchActivation input) =
    singleClassOutput n classes
    where n = cols input
  apply _ _ = ApplyFail ""
  deriv = undefined

prop_correct_forward_pass :: (Positive Int) -> (Positive Int) -> Property
prop_correct_forward_pass (Positive c) (Positive b) = monadicIO $ do
  classes <- liftIO $ randomNNInts (c - 1) b
  let forward = forwardPass [unitCorrect classes]
                            multiNoulli
  activation <- liftIO $ randomMatrix b c
  let input = BatchActivation $ addClasses classes activation
  let pass = yield input
          .| forward
          .| await
  result <- testState pass defTrainState
  assertRight ((== (-1)) . cost . snd) result

multiNoulliTest :: TestTree
multiNoulliTest = $(testGroupGenerator)
