{-# LANGUAGE BangPatterns #-}

module School.FileIO.BinConversion
( binConversion
, decodeList
) where

import Data.ByteString (ByteString, drop, length, take, unpack)
import Numeric.LinearAlgebra (I, R)
import Prelude hiding (drop, length, take)
import School.Types.Decoding (binToInt)
import School.Types.Encoding (Put, putInt, putDouble, runPut)
import School.Types.DataType (DataType(..))

type Converter = ByteString
              -> Either String ByteString

floatMsg :: String
floatMsg = "Illegal conversion fokm floating point to integral"

precMsg :: String
precMsg = "Illegal conversion from higher to lower "
       ++ "precision variables"

convertWords :: (Num a)
             => (Int -> a)
             -> (a -> Put)
             -> Converter
convertWords fromInt put = Right
                         . runPut
                         . (mapM_ put)
                         . map (fromInt . fromEnum)
                         . unpack

decodeList :: Int
           -> (a -> b)
           -> (ByteString -> Either String a)
           -> (ByteString -> Either String [b])
decodeList s convert decode bytes = reverse <$>
  go s bytes [] where
    go n b acc = if length b < n
                   then return acc
                   else do
                     e <- decode . (take n) $ b
                     let e' = convert e
                     go n (drop n b) (e':acc)


binConversion :: DataType
              -> DataType
              -> Converter
binConversion INT08B INT32B =
  convertWords (fromIntegral :: Int -> I) putInt
binConversion INT08B DBL64B =
  convertWords (fromIntegral :: Int -> R) putDouble
binConversion INT32B INT08B = const $ Left precMsg
binConversion INT32B DBL64B = \bytes -> do
  !doubles <- decodeList 4 fromIntegral binToInt bytes
  return . runPut $ mapM_ putDouble doubles

binConversion DBL64B INT08B = const $ Left floatMsg
binConversion DBL64B INT32B = const $ Left floatMsg
binConversion _ _ = Right . id
