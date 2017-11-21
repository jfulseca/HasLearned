{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, Rank2Types, TemplateHaskell #-}

module HML.Types.Test.PosInt
( posIntTest ) where

import Data.ByteString.Conversion (fromByteString, toByteString')
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

prop_parse :: Positive Int -> Bool
prop_parse p@(Positive k) =
  parsed == Just p where
    parsed = fromByteString . toByteString' $ k

prop_parse_negative_fail :: Positive Int -> Bool
prop_parse_negative_fail (Positive k) =
  parsed == Nothing where
    parsed = fromByteString . toByteString' $ (-k) :: Maybe PosInt

prop_extract_PosInts :: Positive Int -> Positive Int -> Bool
prop_extract_PosInts pm@(Positive m) pn@(Positive n) =
  extractPosInts [fst, snd] (m, n) == Just [pm, pn]

prop_extract_PosInts_fail :: Positive Int -> Positive Int -> Bool
prop_extract_PosInts_fail (Positive m) (Positive n) =
  extractPosInts [fst, snd] (m, (-n)) == Nothing

checkBinary :: (Eq b, Num b, Num (Positive b))
            => (forall a. (Num a) => a -> a -> a)
            -> Positive b
            -> Positive b
            -> Bool
checkBinary op pm@(Positive m) pn@(Positive n) =
  (m `op` n) == result
  where (Positive result) = pm `op` pn

checkUnary :: (Eq b, Num b, Num (Positive b))
            => (forall a. (Num a) => a -> a)
            -> Positive b
            -> Bool
checkUnary op pm@(Positive m) =
  (op m) == result
  where (Positive result) = op pm

prop_add :: Positive Int -> Positive Int -> Bool
prop_add = checkBinary (+)

prop_subtract :: Positive Int -> Positive Int -> Bool
prop_subtract = checkBinary (-)

prop_multiply :: Positive Int -> Positive Int -> Bool
prop_multiply = checkBinary (*)

prop_signum :: Positive Int -> Bool
prop_signum = checkUnary signum

prop_negate :: Positive Int -> Bool
prop_negate = checkUnary negate

prop_abs :: Positive Int -> Bool
prop_abs = checkUnary abs

prop_fromInteger :: Integer -> Bool
prop_fromInteger k =
  (Positive . fromInteger $ k) == (fromInteger k :: PosInt)

prop_toRational :: Positive Int -> Bool
prop_toRational p@(Positive k) =
  toRational p == toRational k

prop_toInteger :: Positive Int -> Bool
prop_toInteger p@(Positive k) =
  toInteger p == toInteger k

prop_quotRem :: Positive Int -> Positive Int -> Bool
prop_quotRem pm@(Positive m) pn@(Positive n) =
  (Positive q, Positive r) == pm `quotRem` pn
  where (q, r) = m `quotRem` n

posIntTest :: TestTree
posIntTest = $(testGroupGenerator)
