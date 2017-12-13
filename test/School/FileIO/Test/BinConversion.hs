{-# LANGUAGE TemplateHaskell #-}

module School.FileIO.Test.BinConversion
( binConversionTest ) where

import Data.ByteString (pack, unpack)
import Data.Either (isLeft)
import Data.Word (Word8)
import School.FileIO.BinConversion
import School.Types.Encoding (runPut, putDouble, putInt)
import School.Types.Decoding (binToDouble, binToInt)
import School.Types.TypeName (TypeName(..))
import School.Utils.Either (eitherToBool)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH

prop_word_to_int :: [Word8] -> Bool
prop_word_to_int ws = let
  encoded = pack ws
  ints = map (fromIntegral . fromEnum) ws
  result = do
    converted <- binConversion INT08B INT32B encoded
    decodeList 4 id binToInt converted
  in eitherToBool (== ints) result

prop_word_to_double :: [Word8] -> Bool
prop_word_to_double ws = let
  encoded = pack ws
  doubles = map (fromIntegral . fromEnum) ws
  result = do
    converted <- binConversion INT08B DBL64B encoded
    decodeList 8 id binToDouble converted
  in eitherToBool (== doubles) result

prop_int_to_double :: [Int] -> Bool
prop_int_to_double ints = let
  cints = map fromIntegral ints
  encoded = runPut $ mapM_ putInt cints
  doubles = map fromIntegral ints
  result = do
    converted <- binConversion INT32B DBL64B encoded
    decodeList 8 id binToDouble converted
  in eitherToBool (== doubles) result

prop_word_to_word :: [Word8] -> Bool
prop_word_to_word ws = let
  encoded = pack ws
  result = do
    converted <- binConversion INT08B INT08B encoded
    return $ unpack converted
  in eitherToBool (== ws) result

prop_int_to_int :: [Int] -> Bool
prop_int_to_int ints = let
  cints = map fromIntegral ints
  encoded = runPut $ mapM_ putInt cints
  result = do
    converted <- binConversion INT32B INT32B encoded
    decodeList 4 id binToInt converted
  in eitherToBool (== cints) result

prop_double_to_double :: [Double] -> Bool
prop_double_to_double doubles = let
  encoded = runPut $ mapM_ putDouble doubles
  result = do
    converted <- binConversion DBL64B DBL64B encoded
    decodeList 8 id binToDouble converted
  in eitherToBool (== doubles) result

prop_double_to_integral :: [Double] -> TypeName -> Property
prop_double_to_integral doubles t = (t /= DBL64B) ==> let
  encoded = runPut $ mapM_ putDouble doubles
  result = binConversion DBL64B t encoded
  in isLeft result

prop_int_to_word :: [Int] -> Bool
prop_int_to_word ints = let
  cints = map fromIntegral ints
  encoded = runPut $ mapM_ putInt cints
  result = binConversion INT32B INT08B encoded
  in isLeft result

binConversionTest :: TestTree
binConversionTest = $(testGroupGenerator)
