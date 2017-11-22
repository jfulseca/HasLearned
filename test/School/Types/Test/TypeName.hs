{-# LANGUAGE TemplateHaskell #-}

module School.Types.Test.TypeName
( typeNameTest ) where

import qualified Data.ByteString.Conversion as BC
import School.Types.TypeName
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

prop_equal :: TypeName -> Bool
prop_equal name = name == name

prop_parse_equal :: TypeName -> Bool
prop_parse_equal name = Just name ==
  (BC.fromByteString . BC.toByteString') name

typeNameTest :: TestTree
typeNameTest = $(testGroupGenerator)
