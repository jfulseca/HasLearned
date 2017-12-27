module School.Types.Encoding
( Put
, putDouble
, putInt
, putWord
, doubleToBin
, intToBin
, wordToBin
, matrixDoubleToBin
, matrixIntToBin
, runPut
) where

import Data.ByteString (ByteString)
import Data.Serialize.IEEE754 (putFloat64be)
import Data.Serialize.Put (Put, putInt32be, putWord8, runPut)
import Data.Word (Word8)
import Numeric.LinearAlgebra (Element, I, Matrix, R, toLists)
import School.Types.DataType (DataType(..))

putDouble :: R -> Put
putDouble = putFloat64be

doubleToBin :: R -> ByteString
doubleToBin = runPut . putDouble

putInt :: I -> Put
putInt = putInt32be . fromIntegral

intToBin :: I -> ByteString
intToBin = runPut . putInt

putWord :: Word8 -> Put
putWord = putWord8

wordToBin :: Word8 -> ByteString
wordToBin = runPut . putWord

writer :: (Element b)
       => (a -> Put)
       -> ([b] -> [a])
       -> (Matrix b -> ByteString)
writer put trans matrix = runPut $
  mapM_ put ( trans
            . concat
            . toLists
            $ matrix
            )

matrixDoubleToBin :: DataType
                  -> Matrix R
                  -> ByteString
matrixDoubleToBin DBL64B = writer putDouble id
matrixDoubleToBin INT32B = undefined
matrixDoubleToBin INT08B = undefined

matrixIntToBin :: DataType
               -> Matrix I
               -> ByteString
matrixIntToBin DBL64B =
  writer putDouble (map fromIntegral)
matrixIntToBin INT32B =
  writer putInt id
matrixIntToBin INT08B = undefined
