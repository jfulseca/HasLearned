module School.Types.DoubleToBinary
( doubleFromBinary
, doubleToBinary
) where

import Data.Serialize.Get (Get)
import Data.Serialize.IEEE754 (getFloat64le, putFloat64le)
import Data.Serialize.Put (Put)

doubleFromBinary :: Get Double
doubleFromBinary = getFloat64le

doubleToBinary :: Double -> Put
doubleToBinary = putFloat64le
