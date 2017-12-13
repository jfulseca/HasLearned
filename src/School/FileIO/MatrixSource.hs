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
import School.FileIO.Confirmer (confirmer)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.Decoding (binToMatrixDouble)
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

matrixDoubleSource :: FileType
                   -> MatrixHeader
                   -> FilePath
                   -> MatrixSource Double
matrixDoubleSource fType
                   header@MatrixHeader { cols, rows, dataType }
                   path = do
  let size = rows * cols * getSize dataType
  let confirm = confirmer fType header
  let trans = liftAppS
           . (binToMatrixDouble dataType rows cols)
  sourceFileBS path .| confirm
                    .| poolMatrix size trans

