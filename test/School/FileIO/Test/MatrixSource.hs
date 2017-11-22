{-# LANGUAGE TemplateHaskell #-}

module School.FileIO.Test.MatrixSource
( matrixSourceTest ) where

import Conduit ((.|), sinkList, yield)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.Either (isLeft, isRight)
import Data.Monoid ((<>))
import Data.Serialize.Put (runPut)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource
import School.TestUtils (dummyHeader, dummyList, dummyMatrix, testRun)
import School.Types.DoubleToBinary (doubleToBinary)
import School.Types.PosInt (PosInt)
import School.Types.TypeName (TypeName(..))
import Numeric.LinearAlgebra.Data (Matrix)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pre)

testHeader :: MatrixHeader
           -> ByteString
           -> PropertyM IO (Either String [ByteString])
testHeader header byteString = testRun $
    yield byteString 
 .| confirmMatrixHeader header
 .| sinkList

testMatrix :: PosInt
           -> PosInt
           -> ByteString
           -> PropertyM IO (Either String [Matrix Double])

testMatrix pr pc bytes = testRun $
    yield bytes
 .| poolMatrixDouble pr pc
 .| sinkList

prop_accepts_header :: TypeName -> (Positive Int) -> Property
prop_accepts_header name n =
  monadicIO $ do
    let h = MatrixHeader name n n
    result <- testHeader h (toByteString' h)
    assert $ isRight result

prop_preserves_rest :: String -> Property
prop_preserves_rest content =
  monadicIO $ do
    pre $ content /= ""
    let h = dummyHeader
    let bContent = toByteString' content
    let bytes = (toByteString' h) <> bContent
    result <- testHeader h bytes
    assert $ result == Right [bContent]

prop_rejects_wrong_separator :: Char -> String -> Property
prop_rejects_wrong_separator separator content =
  monadicIO $ do
    pre $ separator /= '#'
    let bytes = (toByteString' separator) <> (toByteString' content)
    result <- testHeader dummyHeader bytes
    assert $ isLeft result

prop_rejects_wrong_header :: TypeName
                        -> TypeName
                        -> (Positive Int)
                        -> (Positive Int)
                        -> (Positive Int)
                        -> (Positive Int)
                        -> Property
prop_rejects_wrong_header name1 name2 n1 n2 n3 n4 =
  monadicIO $ do
    pre $ name1 /= name2
       || mod n3 n1 /= 0
       || n2 /= n4
    let h1 = MatrixHeader name1 n1 n2
    let h2 = MatrixHeader name2 n3 n4
    result <- testHeader h1 (toByteString' h2)
    assert $ isLeft result

encodeDoubles :: [Double] -> ByteString
encodeDoubles =
  foldMap $ \d -> runPut (doubleToBinary d)

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
  let h = MatrixHeader DBL (Positive 3) (Positive 3)
  let path = "test/data/matrix3x3.dat"
  matrix <- testRun $ matrixDoubleSource h path .| sinkList
  let check = Right $ [dummyMatrix 3 3]
  assert $ matrix == check

prop_read_3x3_matrix_twice :: Property
prop_read_3x3_matrix_twice = monadicIO $ do
  let h = MatrixHeader DBL (Positive 3) (Positive 3)
  let path = "test/data/matrix3x3Twice.dat"
  matrix <- testRun $ matrixDoubleSource h path .| sinkList
  let check = dummyMatrix 3 3
  assert $ matrix == Right [check, check]

matrixSourceTest :: TestTree
matrixSourceTest = $(testGroupGenerator)
