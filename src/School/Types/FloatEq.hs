{-# LANGUAGE FlexibleInstances #-}

module School.Types.FloatEq
( FloatEq(..)
, compareDouble
) where

import Numeric.LinearAlgebra (Element)
import Numeric.LinearAlgebra.Data (Matrix, Vector, cols, toList, toLists)
import School.Unit.UnitActivation (UnitActivation(..))

class FloatEq a where
  (~=) :: a -> a -> Bool
  (~/) :: a -> a -> Bool
  x ~= y = not (x ~/ y)
  x ~/ y = not (x ~= y)

instance FloatEq Int where
  (~=) = (==)

instance (FloatEq a) => FloatEq (Maybe a) where
  Nothing ~= Nothing = True
  (Just x) ~= (Just y) = x ~= y
  _ ~= _ = False

compareDouble :: Double -> Double -> Double -> Bool
compareDouble precision d1 d2 = 
  abs ((d2 - d1) / d1) < precision

doubleEq :: Double -> Double -> Bool
doubleEq 0 d = abs d < 5e-11
doubleEq d1 d2 = compareDouble 5e-11 d1 d2

instance FloatEq Double where
  (~=) = doubleEq

instance (FloatEq a) => FloatEq [a] where
  l ~= m = (length l == length m)
        && and (zipWith (~=) l m)

mToList :: (Element a) => Matrix a -> [a]
mToList = concat . toLists

instance (Element a, FloatEq a) => FloatEq (Matrix a) where
  m1 ~= m2 = (cols m1 == cols m2)
          && (mToList m1 ~= mToList m2)

instance (Element a, FloatEq a) => FloatEq (Vector a) where
  v1 ~= v2 = toList v1 ~= toList v2

instance (FloatEq b) => FloatEq (Either a b) where
  (Left _) ~= (Left _) = True
  (Right res1) ~= (Right res2) = res1 ~= res2
  _ ~= _ = False

instance (Element a, FloatEq a) => FloatEq (UnitActivation a) where
  (BatchActivation m1) ~= (BatchActivation m2) = m1 ~= m2
  _ ~= _ = False
