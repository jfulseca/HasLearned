{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.Affine
( affineTest ) where

import Control.Monad.IO.Class (liftIO)
import Numeric.LinearAlgebra ((><), (|>), IndexOf, Matrix, R, Vector, accum,
                              fromRows, ident, size)
import School.TestUtils (CostFunc, diffInput, jTest, matIndexes,
                         randomAffineParams, randomMatrix)
import School.Types.FloatEq ((~=))
import School.Unit.Affine
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Utils.LinearAlgebra (compareDoubleMatrix, compareDoubleVector,
                                   oneMatrix, zeroMatrix, zeroVector)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><), scale)
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO, pre)

eps :: Double
eps = 1e-5

prec :: Double
prec = 5e-2

prop_trivial_apply :: (Positive Int) -> (Positive Int) -> Property
prop_trivial_apply (Positive bSize) (Positive fSize) = monadicIO $ do
  let affineBias = zeroVector fSize
  m <- liftIO $ randomMatrix bSize fSize
  let input = BatchActivation m
  let affineWeights = ident fSize
  let params = AffineParams { affineBias
                            , affineWeights }
  let result = apply affine params input
  assert $ result ~= input

randomSetup :: Int
            -> Int
            -> Int
            -> IO (UnitParams R, UnitActivation R)
randomSetup bSize fSize oSize = do
  input <- randomMatrix bSize fSize
  affineParams <- randomAffineParams fSize oSize
  return ( affineParams
         , BatchActivation input
         )

prop_overwrite_apply :: (Positive Int) -> (Positive Int) -> Positive Int -> Property
prop_overwrite_apply (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  let affineWeights = zeroMatrix oSize fSize
  (params, input) <- liftIO $ randomSetup bSize fSize oSize
  let params' = params { affineWeights }
  let result = apply affine params' input
  let check = fromRows . (replicate bSize) $ affineBias params
  assert $ result ~= BatchActivation check

prop_deriv_dims :: (Positive Int) -> (Positive Int) -> Positive Int -> Property
prop_deriv_dims (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  (params, input) <- liftIO $ randomSetup bSize fSize oSize
  gradIn <- liftIO $ fmap BatchGradient $ randomMatrix bSize oSize
  let (BatchGradient gradOut, AffineParams{ affineBias, affineWeights }) = deriv affine params gradIn input
  assert $ (size gradOut == (bSize, fSize))
        && (size affineBias == oSize)
        && (size affineWeights == (oSize, fSize))

type AlterParams a = Double
                  -> IndexOf a
                  -> UnitParams R
                  -> UnitParams R
 
diffBias :: UnitParams R
          -> UnitActivation R
          -> CostFunc
          -> IndexOf Vector
          -> Double
diffBias params input cost idx =
  (jAdd - jSub) / (2*eps) where
    outAdd = apply affine (alterBias eps idx params) input
    outSub = apply affine (alterBias (-eps) idx params) input
    jAdd = cost outAdd
    jSub = cost outSub

diffWeights :: UnitParams R
            -> UnitActivation R
            -> CostFunc
            -> IndexOf Matrix
            -> Double
diffWeights params input cost idx =
  (jAdd - jSub) / (2*eps) where
    outAdd = apply affine (alterWeights eps idx params) input
    outSub = apply affine (alterWeights (-eps) idx params) input
    jAdd = cost outAdd
    jSub = cost outSub

alterBias :: AlterParams Vector
alterBias change idx AffineParams{ affineBias, affineWeights } =
  let newBias = accum affineBias (+) [(idx, change)]
  in AffineParams { affineBias = newBias, affineWeights }
alterBias _ _ _ = EmptyParams

alterWeights :: AlterParams Matrix
alterWeights change idx AffineParams{ affineBias, affineWeights } =
  let newWeights = accum affineWeights (+) [(idx, change)]
  in AffineParams { affineBias, affineWeights = newWeights }
alterWeights _ _ _ = EmptyParams

vecIndexes :: Int -> [IndexOf Vector]
vecIndexes n = [0..n-1]

prop_numerical_gradient :: (Positive Int) -> (Positive Int) -> Positive Int -> Property
prop_numerical_gradient (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  pre $ bSize < 50 && fSize < 50 && oSize < 50
  (params, input) <- liftIO $ randomSetup bSize fSize oSize
  let idxs = matIndexes bSize fSize
  let num = map (\idx -> diffInput affine params input eps idx)
                idxs
  let numGrad = (bSize >< fSize) num
  let inGrad = BatchGradient $ oneMatrix bSize oSize
  let (BatchGradient result, _) = deriv affine params inGrad input
  assert $ compareDoubleMatrix prec result numGrad

prop_numerical_bias_deriv :: (Positive Int) -> (Positive Int) -> Positive Int -> Property
prop_numerical_bias_deriv (Positive bSize) (Positive fSize) (Positive oSize) = (mapSize (const 10)) . monadicIO $ do
  pre $ bSize < 50 && fSize < 50 && oSize < 50
  (params, input) <- liftIO $ randomSetup bSize fSize oSize
  let idxs = vecIndexes oSize
  let num = map (\idx -> diffBias params input jTest idx)
                idxs
  let numDeriv = oSize |> num
  let inGrad = BatchGradient $ oneMatrix bSize oSize
  let (_, AffineParams { affineBias }) = deriv affine params inGrad input
  assert $ compareDoubleVector prec affineBias numDeriv

prop_numerical_weights_deriv :: (Positive Int) -> (Positive Int) -> Positive Int -> Property
prop_numerical_weights_deriv (Positive bSize) (Positive fSize) (Positive oSize) = (mapSize (const 10)) . monadicIO $ do
  pre $ bSize < 50 && fSize < 50 && oSize < 50
  (params, input) <- liftIO $ randomSetup bSize fSize oSize
  let idxs = matIndexes oSize fSize
  let num = map (\idx -> diffWeights params input jTest idx)
                idxs
  let numDeriv = (oSize >< fSize) num
  let inGrad = BatchGradient $ oneMatrix bSize oSize
  let (_, AffineParams { affineWeights }) = deriv affine params inGrad input
  assert $ compareDoubleMatrix prec affineWeights numDeriv

affineTest :: TestTree
affineTest = $(testGroupGenerator)
