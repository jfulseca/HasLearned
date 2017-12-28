{-# LANGUAGE FlexibleInstances #-}

module School.Types.FloatEq
( FloatEq(..)
, compareDouble
) where

import Numeric.LinearAlgebra (Element)
import Numeric.LinearAlgebra.Data (Matrix, Vector, cols, toList, toLists)
import School.Train.TrainState (HandlerStore(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))

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
compareDouble prec d1 d2 = if abs d1 < prec
  then abs d2 < prec
  else abs ((d2 - d1) / d1) < prec

doubleEq :: Double -> Double -> Bool
doubleEq = compareDouble 5e-11

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

instance (Element a, FloatEq a) => FloatEq (UnitGradient a) where
  (BatchGradient g1) ~= (BatchGradient g2) = g1 ~= g2
  _ ~= _ = False

instance (Element a, FloatEq a) => FloatEq (UnitParams a) where
  AffineParams { affineBias = b1, affineWeights = w1 } ~= AffineParams { affineBias = b2, affineWeights = w2 } =
    b1 ~= b2 && w1 ~= w2
  EmptyParams ~= EmptyParams = True
  _ ~= _ = False

instance (FloatEq a) => FloatEq (HandlerStore a) where
  (CostList l1) ~= (CostList l2) = l1 ~= l2
  NoStore ~= NoStore = True
  _ ~= _ = False
