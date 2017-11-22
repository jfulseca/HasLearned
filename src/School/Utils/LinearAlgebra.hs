{-# LANGUAGE FlexibleContexts #-}

module School.Utils.LinearAlgebra
( (+>)
, oneVector
, zeroMatrix
, zeroVector
) where

import Numeric.LinearAlgebra

-- Adds vector to each row
(+>) :: (Element a, Container Vector a)
     => Matrix a
     -> Vector a
     -> Matrix a
(+>) m v =
  fromRows $ map (add v) (toRows m)

oneVector :: Int -> Vector R
oneVector d = konst 1 d

zeroVector :: Int -> Vector R
zeroVector d = konst 0 d

zeroMatrix :: Int -> Int -> Matrix R
zeroMatrix r c = konst 0 (r, c)
