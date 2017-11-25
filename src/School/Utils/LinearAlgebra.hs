{-# LANGUAGE FlexibleContexts #-}

module School.Utils.LinearAlgebra
( (+>)
, compareDoubleMatrix
, compareDoubleVector
, constVector
, oneMatrix
, oneVector
, sumCols
, zeroMatrix
, zeroVector
) where

import Numeric.LinearAlgebra
import School.Types.FloatEq (compareDouble)

-- Adds vector to each row
(+>) :: (Element a, Container Vector a)
     => Matrix a
     -> Vector a
     -> Matrix a
(+>) m v =
  fromRows $ map (add v) (toRows m)

sumCols :: Matrix R -> Vector R
sumCols m = n |> summedCols where
  n = cols m
  summedCols = map sumElements $ toColumns m

oneVector :: Int -> Vector R
oneVector d = konst 1 d

constVector :: Int -> Double -> Vector R
constVector d v = konst v d

zeroVector :: Int -> Vector R
zeroVector d = konst 0 d

zeroMatrix :: (Container Vector a, Num a)
           => Int
           -> Int
           -> Matrix a
zeroMatrix r c = konst 0 (r, c)

oneMatrix :: Int -> Int -> Matrix R
oneMatrix r c = konst 1 (r, c)

compareDoubleMatrix :: Double
                    -> Matrix R
                    -> Matrix R
                    -> Bool
compareDoubleMatrix prec m1 m2 =
  and $ zipWith (compareDouble prec) l1 l2
  where l1 = concat . toLists $ m1
        l2 = concat . toLists $ m2

compareDoubleVector :: Double
                    -> Vector R
                    -> Vector R
                    -> Bool
compareDoubleVector prec v1 v2 =
  and $ zipWith (compareDouble prec) l1 l2
  where l1 = toList $ v1
        l2 = toList $ v2
