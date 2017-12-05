{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.LogSoftMax
( logSoftMaxTest ) where

import Control.Monad.IO.Class (liftIO)
import Numeric.LinearAlgebra ((><), size, toLists)
import School.TestUtils (diffInput, matIndexes, randomMatrix, randomMatrixL)
import School.Types.FloatEq ((~=))
import School.Unit.LogSoftMax
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Utils.LinearAlgebra (compareDoubleMatrix, oneMatrix)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><), scale)
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO, pre)

eps :: Double
eps = 1e-5 

prec :: Double
prec = 1e-3

prop_norm_size :: (Positive Int) -> (Positive Int) -> Property
prop_norm_size (Positive bSize) (Positive fSize) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrixL 5 bSize fSize
  let (BatchActivation result) = apply logSoftMax EmptyParams input
  assert $ and . (map (<= prec)) . concat . toLists $ result

prop_norm_sum :: (Positive Int) -> (Positive Int) -> Property
prop_norm_sum (Positive bSize) (Positive fSize) = monadicIO $ do
  input <- liftIO $ BatchActivation <$> randomMatrixL 5 bSize fSize
  let (BatchActivation result) = apply logSoftMax EmptyParams input
  assert $ and
         . (map (~= 1))
         . (map (sum . (map exp)))
         . toLists
         $ result

prop_gradient_dims :: (Positive Int) -> Positive Int -> Property
prop_gradient_dims (Positive bSize) (Positive fSize) = monadicIO $ do
  input <- liftIO $ fmap BatchActivation $ randomMatrix bSize fSize
  gradIn <- liftIO $ fmap BatchGradient $ randomMatrix bSize fSize
  let (BatchGradient gradOut, _) = deriv logSoftMax EmptyParams gradIn input
  assert $ (size gradOut == (bSize, fSize))

prop_numerical_gradient :: (Positive Int) -> Positive Int -> Property
prop_numerical_gradient (Positive bSize) (Positive fSize) = monadicIO $ do
  pre $ bSize < 25 && fSize < 25
  input <- liftIO $ fmap BatchActivation $ randomMatrixL 5 bSize fSize
  let params = EmptyParams
  let idxs = matIndexes bSize fSize
  let num = map (\idx -> diffInput logSoftMax params input eps idx)
                idxs
  let numGrad = (bSize >< fSize) num
  let inGrad = BatchGradient $ oneMatrix bSize fSize
  let (BatchGradient result, _) = deriv logSoftMax params inGrad input
  assert $ compareDoubleMatrix prec result numGrad

logSoftMaxTest :: TestTree
logSoftMaxTest = $(testGroupGenerator)
