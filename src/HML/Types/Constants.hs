module HML.Types.Constants
( binComma
, binEndl
, binarySeparator
, separator ) where

import Data.Char (ord)
import GHC.Word (Word8)

separator :: Char
separator = '#'

toWord8 :: Char -> Word8
toWord8 = toEnum . ord

binarySeparator :: Word8
binarySeparator = toWord8 separator

binEndl :: Word8
binEndl = toWord8 '\n'

binComma :: Word8
binComma = toWord8 ','
