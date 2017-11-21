{-# LANGUAGE TemplateHaskell #-}

module HML.Types.Test.FloatEq
( floatEqTest ) where

import Data.Either (isLeft)
import HML.Types.FloatEq
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH

prop_int_equal :: Int -> Bool
prop_int_equal k = k ~= k

prop_int_unequal :: Int -> Int -> Property
prop_int_unequal m n = (m /= n) ==> m ~/ n

prop_maybe_int_equal :: Maybe Int -> Bool
prop_maybe_int_equal k = k ~= k

prop_maybe_int_unequal :: Maybe Int -> Maybe Int -> Property
prop_maybe_int_unequal m n = (m /= n) ==> m ~/ n

prop_either_int_equal :: Either String Int -> Bool
prop_either_int_equal k = k ~= k

prop_either_int_unequal :: Either String Int -> Either String Int -> Property
prop_either_int_unequal m n = (m /= n) ==>
  (isLeft m && isLeft n) || m ~/ n

prop_double_equal :: Double -> Bool 
prop_double_equal k = k ~= k

floatEqTest :: TestTree
floatEqTest= $(testGroupGenerator)
