{-# LANGUAGE TemplateHaskell #-}

module School.FileIO.Test.MatrixSink
( matrixSinkTest ) where

import Conduit ((.|), runConduit, yield, yieldMany)
import Data.Either (isRight)
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSink
import School.TestUtils (dummyMatrix)
import School.Types.DataType (DataType(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH

prop_write_3x3_matrix :: Property
prop_write_3x3_matrix = monadicIO $ do
  let header = MatrixHeader DBL64B 3 3
  let matrix = dummyMatrix 3 3
  let path = "test/data/matrix3x3.sm"
  result <- run . runAppIO . runConduit $
    yield matrix .| matrixDoubleSink SM header path
  assert $ isRight result

prop_write_3x3_matrix_twice :: Property
prop_write_3x3_matrix_twice = monadicIO $ do
  let header = MatrixHeader DBL64B 6 3
  let matrix = dummyMatrix 3 3
  let path = "test/data/matrix3x3Twice.sm"
  result <- run . runAppIO . runConduit $
      yieldMany [matrix, matrix]
   .| matrixDoubleSink SM header path
  assert $ isRight result

matrixSinkTest :: TestTree
matrixSinkTest = $(testGroupGenerator)
