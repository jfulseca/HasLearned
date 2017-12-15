{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.MatrixSourcery
( MatrixConduit
, MatrixSourcery
, matrixDoubleSourcery
, poolMatrix
) where

import Conduit (($$+), (.|), ConduitM, mapMC,
                nullC, sourceFileBS, takeCE)
import Data.ByteString (ByteString)
import School.App.AppS (AppS, liftAppS)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.Decoding (binToMatrixDouble)
import School.Types.DataType (getSize)
import School.Types.Sourcery (Sourcery, sourcery)
import Numeric.LinearAlgebra (Element, Matrix, R)

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

type MatrixSourcery a r = Sourcery (Matrix a)
                                   (AppS a)
                                   r

matrixDoubleSourcery :: FileType
                     -> MatrixHeader
                     -> FilePath
                     -> MatrixSourcery R r
matrixDoubleSourcery fType
                     header@MatrixHeader { cols, rows, dataType }
                     path
                     sink = do
  let size = rows * cols * getSize dataType
  let cHeader = conduitHeader fType header
  let trans = liftAppS
           . (binToMatrixDouble dataType rows cols)
  (resumable, _) <- sourceFileBS path $$+ cHeader
  sourcery resumable (poolMatrix size trans) sink

--  let poolSink = poolMatrix size trans .| sink
--  resumable $$+- poolSink
