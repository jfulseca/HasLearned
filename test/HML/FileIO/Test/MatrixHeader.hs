{-# LANGUAGE TemplateHaskell #-}
module HML.FileIO.Test.MatrixHeader
( dummyHeader
, matrixHeaderTest
) where

import Data.ByteString.Conversion (toByteString')
import HML.FileIO.MatrixHeader
import HML.Types.TypeName (TypeName(..))
import Test.QuickCheck.Modifiers (Positive(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

dummyHeader :: MatrixHeader
dummyHeader = MatrixHeader INT n n where
  n = Positive 1

prop_parse_equal :: TypeName -> Positive Int -> Positive Int -> Bool
prop_parse_equal name nRows nCols =
  Right header == parsedHeader where
    header = MatrixHeader name nRows nCols
    parsedHeader = (stripSeparators . toByteString') header

prop_compatible_check :: TypeName -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_compatible_check name nCols nRows1@(Positive r1) nRows2@(Positive r2) =
  let h1 = MatrixHeader name nRows1 nCols
      h2 = MatrixHeader name nRows2  nCols
      comp = r2 `mod` r1 == 0 in
  comp == compatibleHeaders h1 h2

matrixHeaderTest :: TestTree
matrixHeaderTest = $(testGroupGenerator)
