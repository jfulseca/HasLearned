module School.Utils.Double
( doubleRange ) where

doubleZero :: Double
doubleZero = 0

doubleRange :: (Double, Double)
doubleRange = let
  b = floatRadix doubleZero
  e = floatDigits doubleZero
  (_, e') = floatRange doubleZero
  m = b ^ e - 1
  n = e' - e
  p = encodeFloat m n
  in (-p, p)
