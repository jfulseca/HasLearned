{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.RecLin
( recLinTest ) where

import Control.Monad.IO.Class (liftIO)
import Numeric.LinearAlgebra ((><), fromLists, size, toLists)
import School.TestUtils (diffInput, matIndexes, randomMatrix)
import School.Types.FloatEq ((~=))
import School.Unit.RecLin
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Utils.LinearAlgebra (compareDoubleMatrix, oneMatrix)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><), scale)
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

eps :: Double
eps = 1e-5 

prec :: Double
prec = 1e-4

prop_apply :: (Positive Int) -> (Positive Int) -> Property
prop_apply (Positive bSize) (Positive fSize) = monadicIO $ do
  m <- liftIO $ randomMatrix bSize fSize
  let input = BatchActivation m
  let params = EmptyParams
  let (BatchActivation result) = apply recLin params input
  let check = fromLists $ map (map (max 0)) $ toLists m
  assert $ result ~= check

prop_gradient_dims :: (Positive Int) -> Positive Int -> Property
prop_gradient_dims (Positive bSize) (Positive fSize) = monadicIO $ do
  input <- liftIO $ fmap BatchActivation $ randomMatrix bSize fSize
  gradIn <- liftIO $ fmap BatchGradient $ randomMatrix bSize fSize
  let params = EmptyParams
  let (BatchGradient gradOut, _) = deriv recLin params gradIn input
  assert $ (size gradOut == (bSize, fSize))

prop_numerical_gradient :: (Positive Int) -> Positive Int -> Property
prop_numerical_gradient (Positive bSize) (Positive fSize) = monadicIO $ do
  input <- liftIO $ fmap BatchActivation $ randomMatrix bSize fSize
  let params = EmptyParams
  let idxs = matIndexes bSize fSize
  let num = map (\idx -> diffInput recLin params input eps idx)
                idxs
  let numGrad = (bSize >< fSize) num
  let inGrad = BatchGradient $ oneMatrix bSize fSize
  let (BatchGradient result, _) = deriv recLin params inGrad input
  assert $ compareDoubleMatrix prec result numGrad

recLinTest :: TestTree
recLinTest = $(testGroupGenerator)
