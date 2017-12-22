{-# LANGUAGE TemplateHaskell #-}

module School.FileIO.Test.MatrixSourcery
( matrixSourceryTest ) where

import Conduit (($$+-), (.|), runConduit, sinkList, yield)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.Either (isLeft, isRight)
import Data.Monoid ((<>))
import School.FileIO.AppIO (AppIO, runAppIO)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileHeader (FileHeader(..))
import School.FileIO.MatrixSourcery
import School.FileIO.FileType (FileType(..))
import School.TestUtils (def, dummyList, dummyMatrix)
import School.Types.Decoding (binToMatrixDouble)
import School.Types.Encoding (doubleToBin)
import School.Types.DataType (DataType(..), getSize)
import School.Types.LiftResult (liftResult)
import School.Types.Slinky (Slinky(..))
import School.Unit.UnitActivation (UnitActivation(..))
import Numeric.LinearAlgebra.Data (Matrix)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pre, run)

testHeader :: FileHeader
           -> ByteString
           -> PropertyM IO (Either String [ByteString])
testHeader header byteString = run . runAppIO $ do
  let source = yield byteString
  cHeader <- conduitHeader SM header source
  cHeader $$+- sinkList

poolMatrixDouble :: Positive Int
                 -> Positive Int
                 -> MatrixConduit AppIO Double
poolMatrixDouble (Positive r) (Positive c) =
  let trans = liftResult
            . (binToMatrixDouble DBL64B r c)
      size = getSize DBL64B
  in poolMatrix (r * c * size) trans

testMatrix :: Positive Int
           -> Positive Int
           -> ByteString
           -> PropertyM IO (Either String [Matrix Double])

testMatrix pr pc bytes = run . runAppIO . runConduit $
    yield bytes
 .| poolMatrixDouble pr pc
 .| sinkList

prop_accepts_header :: DataType -> (Positive Int) -> Property
prop_accepts_header name (Positive n) =
  monadicIO $ do
    let h = FileHeader name n n
    result <- testHeader h (toByteString' h)
    assert $ isRight result

prop_preserves_rest :: String -> Property
prop_preserves_rest content =
  monadicIO $ do
    pre $ content /= ""
    let h = def
    let bContent = toByteString' content
    let bytes = (toByteString' h) <> bContent
    result <- testHeader h bytes
    assert $ result == Right [bContent]

prop_rejects_wrong_separator :: Char -> String -> Property
prop_rejects_wrong_separator separator content =
  monadicIO $ do
    pre $ separator /= '#'
    let bytes = (toByteString' separator) <> (toByteString' content)
    result <- testHeader def bytes
    assert $ isLeft result

prop_rejects_wrong_header :: DataType
                          -> DataType
                          -> (Positive Int)
                          -> (Positive Int)
                          -> (Positive Int)
                          -> (Positive Int)
                          -> Property
prop_rejects_wrong_header name1
                          name2
                          (Positive n1)
                          (Positive n2)
                          (Positive n3)
                          (Positive n4) =
  monadicIO $ do
    pre $ name1 /= name2
       || mod n3 n1 /= 0
       || n2 /= n4
    let h1 = FileHeader name1 n1 n2
    let h2 = FileHeader name2 n3 n4
    result <- testHeader h1 (toByteString' h2)
    assert $ isLeft result

encodeDoubles :: [Double] -> ByteString
encodeDoubles = foldMap doubleToBin

prop_read_double_matrix :: Positive Int
                      -> Positive Int
                      -> Property
prop_read_double_matrix pr@(Positive r) pc@(Positive c) =
  monadicIO $ do
    let bytes = encodeDoubles $ dummyList r c
    matrix <- testMatrix pr pc bytes
    assert $ matrix == Right [dummyMatrix r c]

prop_read_two_double_matrices :: Positive Int
                           -> Positive Int
                           -> Property
prop_read_two_double_matrices pr@(Positive r) pc@(Positive c) =
  monadicIO $ do
    let list = dummyList r c
    let bytes = encodeDoubles $ list <> list
    matrices <- testMatrix pr pc bytes
    let check = dummyMatrix r c
    assert $ matrices == Right [check, check]

prop_read_with_extra :: Positive Int
                   -> Positive Int
                   -> Property
prop_read_with_extra pr@(Positive r) pc@(Positive c) =
  monadicIO $ do
    pre $ c > 1
    let extra = encodeDoubles [3]
    let bytes = encodeDoubles $ dummyList r c
    matrix <- testMatrix pr pc (bytes <> extra)
    assert $ isLeft matrix

prop_read_3x3_matrix :: Property
prop_read_3x3_matrix = monadicIO $ do
  let h = FileHeader DBL64B 3 3
  let path = "test/data/matrix3x3.sm"
  stack <- run . runAppIO $
    matrixDoubleSourcery SM h path id sinkList
  let check = Right $ [([BatchActivation $ dummyMatrix 3 3], SNil)]
  assert $ stack == check

prop_read_3x3_matrix_twice :: Property
prop_read_3x3_matrix_twice = monadicIO $ do
  let h = FileHeader DBL64B 3 3
  let path = "test/data/matrix3x3Twice.sm"
  stack <- run . runAppIO $
    matrixDoubleSourcery SM h path id sinkList
  let check = ([BatchActivation $ dummyMatrix 3 3], SNil)
  assert $ stack == Right [check, check]

matrixSourceryTest :: TestTree
matrixSourceryTest = $(testGroupGenerator)
