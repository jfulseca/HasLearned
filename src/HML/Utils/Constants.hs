module HML.Utils.Constants
( binComma
, binEndl
, binSeparator
, doubleSize
, separator
) where

import Data.Char (ord)
import Foreign.Storable (sizeOf)
import GHC.Word (Word8)

doubleSize :: Int
doubleSize = sizeOf (0 :: Double)

separator :: Char
separator = '#'

toWord8 :: Char -> Word8
toWord8 = toEnum . ord

binSeparator :: Word8
binSeparator = toWord8 separator

binEndl :: Word8
binEndl = toWord8 '\n'

binComma :: Word8
binComma = toWord8 ','
