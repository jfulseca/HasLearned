{-# LANGUAGE TemplateHaskell #-}

module HML.FileIO.Test.MatrixSink
( matrixSinkTest ) where

import Conduit ((.|), yield, yieldMany)
import Data.Either (isRight)
import HML.FileIO.MatrixHeader (MatrixHeader(..))
import HML.FileIO.MatrixSink
import HML.TestUtils (dummyMatrix, testRun)
import HML.Types.TypeName (TypeName(..))
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH

prop_write_3x3_matrix :: Property
prop_write_3x3_matrix = monadicIO $ do
  let p = Positive 3
  let header = MatrixHeader DBL p p
  let matrix = dummyMatrix 3 3
  let path = "test/data/matrix3x3.dat"
  result <- testRun $ yield matrix .| matrixDoubleSink header path
  assert $ isRight result

prop_write_3x3_matrix_twice :: Property
prop_write_3x3_matrix_twice = monadicIO $ do
  let pr = Positive 6
  let pc = Positive 3
  let header = MatrixHeader DBL pr pc
  let matrix = dummyMatrix 3 3
  let path = "test/data/matrix3x3Twice.dat"
  result <- testRun $ yieldMany [matrix, matrix] .| matrixDoubleSink header path
  assert $ isRight result

matrixSinkTest :: TestTree
matrixSinkTest = $(testGroupGenerator)
