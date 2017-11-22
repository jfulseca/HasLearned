module School.Types.DoubleConversion
( fromBinary
, getDouble
, putDouble
, toBinary
) where

import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.IEEE754 (getFloat64le, putFloat64le)
import Data.Serialize.Put (Put, runPut)

putDouble :: Double -> Put
putDouble = putFloat64le

getDouble :: Get Double
getDouble = getFloat64le

toBinary :: Double -> ByteString
toBinary = runPut . putDouble

fromBinary :: ByteString -> Either String Double
fromBinary = runGet getDouble
