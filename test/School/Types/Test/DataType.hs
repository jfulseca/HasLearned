{-# LANGUAGE TemplateHaskell #-}

module School.Types.Test.DataType
( typeNameTest ) where

import qualified Data.ByteString.Conversion as BC
import School.Types.DataType
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

prop_equal :: DataType -> Bool
prop_equal name = name == name

prop_parse_equal :: DataType -> Bool
prop_parse_equal name = Just name ==
  (BC.fromByteString . BC.toByteString') name

typeNameTest :: TestTree
typeNameTest = $(testGroupGenerator)
