{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.MatrixSource
( MatrixConduit
, MatrixSource
, matrixDoubleSource
, poolMatrix
) where

import Conduit ((.|), ConduitM, mapMC, nullC, sourceFileBS, takeCE)
import Data.ByteString (ByteString)
import School.App.AppS (AppS, liftAppS)
import School.FileIO.Confirmer (Confirmer)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.DoubleConversion (toMatrixDouble)
import School.Types.PosInt (getPosInt)
import School.Types.TypeName (getSize)
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

matrixDoubleSource :: (MatrixHeader -> Confirmer Double)
                   -> MatrixHeader
                   -> FilePath
                   -> MatrixSource Double
matrixDoubleSource confirmer header path = do
  let r = getPosInt . rows $ header
  let c = getPosInt . cols $ header
  let t = dataType header
  let s = getSize t
  let trans = liftAppS
           . (toMatrixDouble t r c)
  sourceFileBS path .| confirmer header
                    .| poolMatrix (r * c * s) trans

