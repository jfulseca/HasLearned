{-# LANGUAGE TemplateHaskell #-}

module School.Unit.Test.WeightDecay
( weightDecayTest ) where

import Conduit(Identity, liftIO)
import Numeric.LinearAlgebra ((><))
import School.TestUtils (diffCost, randomMatrix)
import School.Types.FloatEq ((~=))
import School.Types.Slinky (Slinky(..))
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.WeightDecay
import School.Utils.LinearAlgebra (compareDoubleMatrix)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (monadicIO, assert, pre)

eps :: Double
eps = 1e-5

prec :: Double
prec = 5e-2

weight :: Double -> CostFunction Double Identity
weight coeff = weightDecay coeff

prop_coeff_zero :: (Positive Int) -> (Positive Int) -> Property
prop_coeff_zero (Positive bSize) (Positive fSize) = monadicIO $ do
  inputMat <- liftIO $ randomMatrix bSize fSize
  let input = BatchActivation inputMat
  let cost = computeCost (weight 0) input SNil
  either (const (assert False))
         (\result -> assert $ result ~= 0)
         cost

prop_numerical_deriv :: (Positive Int) -> (Positive Int) -> Double -> Property
prop_numerical_deriv (Positive bSize) (Positive fSize) coeff = monadicIO $ do
  pre $ bSize < 23 && fSize < 23
  inputMat <- liftIO $ randomMatrix bSize fSize
  let input = BatchActivation inputMat
  let costFunc = weight coeff
  let deriv = derivCost costFunc input SNil
  let check = (bSize >< fSize) [ diffCost costFunc
                                          input
                                          eps
                                          (j, k)
                                          SNil
                               | j <- [0..bSize-1], k <- [0..fSize-1] ]
  either (const (assert False))
         (\(BatchGradient g) -> assert $ compareDoubleMatrix prec g check)
         deriv

weightDecayTest :: TestTree
weightDecayTest = $(testGroupGenerator)
