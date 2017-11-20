{-# LANGUAGE TemplateHaskell #-}

module HML.Types.Test.PosInt
( posIntTest ) where

import HML.Types.PosInt
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH

prop_positive_unchanged :: Positive Int -> Bool
prop_positive_unchanged (Positive k) =
  maybe False ((==k) . getPosInt) $ posInt k

prop_non_positive_nothing :: (NonNegative Int) -> Bool
prop_non_positive_nothing (NonNegative k) =
  maybe True (const False) $ posInt (-k)

prop_equal :: Int -> Bool
prop_equal k = (posInt k) == (posInt k)

prop_different :: (Positive Int) -> (Positive Int) -> Property
prop_different (Positive m) (Positive n) = (m /= n) ==>
  (posInt m) /= (posInt n)

posIntTest :: TestTree
posIntTest = $(testGroupGenerator)
