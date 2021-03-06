{-# LANGUAGE TemplateHaskell #-}
module School.FileIO.Test.FileHeader
( matrixHeaderTest ) where

import Data.ByteString.Conversion (toByteString')
import School.FileIO.FileHeader
import School.Types.DataType (DataType(..))
import Test.QuickCheck.Modifiers (Positive(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

prop_parse_equal :: DataType -> Positive Int -> Positive Int -> Bool
prop_parse_equal name (Positive r) (Positive c) =
  Right header == parsedHeader where
    header = FileHeader name r c
    parsedHeader = (parseHeader . toByteString') header

prop_compatible_check :: DataType -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_compatible_check name (Positive c) (Positive r1) (Positive r2) =
  let h1 = FileHeader name r1 c
      h2 = FileHeader name r2 c
      comp = r2 `mod` r1 == 0 in
  comp == compatibleHeaders h1 h2

matrixHeaderTest :: TestTree
matrixHeaderTest = $(testGroupGenerator)
