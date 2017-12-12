module School.FileIO.MatrixSink
( MatrixSink
, matrixDoubleSink
, matrixIntSink
) where

import Conduit ((.|), ConduitM, mapC, sinkFileBS, yield)
import Data.ByteString (ByteString)
import Data.Void (Void)
import School.App.AppS (AppS)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.Encoding (matrixDoubleToBin, matrixIntToBin)
import Numeric.LinearAlgebra.Data (I, Matrix, R)

type MatrixSink a = ConduitM (Matrix a)
                             Void
                             (AppS a)
                             ()

matrixDoubleSink :: (MatrixHeader -> ByteString)
                 -> MatrixHeader
                 -> FilePath
                 -> MatrixSink R
matrixDoubleSink headerBuild header path =
  byteBuilder .| sinkFileBS path where
    dType = dataType header
    byteBuilder = do
      yield $ headerBuild header
      mapC (matrixDoubleToBin dType)

matrixIntSink :: (MatrixHeader -> ByteString)
              -> MatrixHeader
              -> FilePath
              -> MatrixSink I
matrixIntSink headerBuild header path =
  byteBuilder .| sinkFileBS path where
    dType = dataType header
    byteBuilder = do
      yield $ headerBuild header
      mapC (matrixIntToBin dType)
