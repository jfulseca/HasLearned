{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.Affine
( affineTest ) where

import Control.Monad.IO.Class (liftIO)
import Numeric.LinearAlgebra (fromRows, ident)
import School.TestUtils (randomMatrix, randomVector)
import School.Types.FloatEq ((~=))
import School.Unit.Affine
import School.Unit.Constituents (Constituents(..))
import School.Unit.Unit (Unit(..))
import School.Utils.LinearAlgebra (zeroMatrix, zeroVector)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

prop_trivial_op :: (Positive Int) -> (Positive Int) -> Property
prop_trivial_op (Positive bSize) (Positive fSize) = monadicIO $ do
  let biasVector = zeroVector fSize
  inputMatrix <- liftIO $ randomMatrix bSize fSize
  let weightMatrix = ident fSize
  let consts = AffineConstituents { biasVector
                                  , inputMatrix
                                  , weightMatrix
                                  }
  let result = op affine consts
  assert $ result ~= inputMatrix

prop_overwrite_op :: (Positive Int) -> (Positive Int) -> Positive Int -> Property
prop_overwrite_op (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  biasVector <- liftIO $ randomVector oSize
  inputMatrix <- liftIO $ randomMatrix bSize fSize
  let weightMatrix = zeroMatrix oSize fSize
  let consts = AffineConstituents { biasVector
                                  , inputMatrix
                                  , weightMatrix
                                  }
  let result = op affine consts
  let check = fromRows . (replicate bSize) $ biasVector
  assert $ result ~= check

affineTest :: TestTree
affineTest = $(testGroupGenerator)
