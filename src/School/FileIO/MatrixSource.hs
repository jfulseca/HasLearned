{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.MatrixSource
( MatrixConduit
, MatrixSource
, poolMatrix
) where

import Conduit ((.|), ConduitM, mapMC, nullC, takeCE)
import Data.ByteString (ByteString)
import School.App.AppS (AppS)
import Numeric.LinearAlgebra (Element, Matrix)

type MatrixSource a =
  ConduitM () (Matrix a) (AppS a) ()

type MatrixConduit a = ConduitM ByteString
                                (Matrix a)
                                (AppS a)
                                ()

poolMatrix :: (Element a)
           => Int
           -> (ByteString -> AppS a (Matrix a))
           -> MatrixConduit a
poolMatrix chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop
