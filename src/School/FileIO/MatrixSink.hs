module School.FileIO.MatrixSink
( MatrixSink
, matrixDoubleSink
, matrixIntSink
) where

import Conduit ((.|), ConduitM, mapC, sinkFileBS, yield)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO)
import School.FileIO.FileType (FileType)
import School.FileIO.MatrixHeader (MatrixHeader(..), headerBuilder)
import School.Types.Encoding (matrixDoubleToBin, matrixIntToBin)
import Numeric.LinearAlgebra.Data (I, Matrix, R)

type MatrixSink a = ConduitM (Matrix a)
                             Void
                             AppIO
                             ()

matrixDoubleSink :: FileType
                 -> MatrixHeader
                 -> FilePath
                 -> MatrixSink R
matrixDoubleSink fType header path =
  byteBuilder .| sinkFileBS path where
    dType = dataType header
    byteBuilder = do
      yield $ headerBuilder fType header
      mapC (matrixDoubleToBin dType)

matrixIntSink :: FileType
              -> MatrixHeader
              -> FilePath
              -> MatrixSink I
matrixIntSink fType header path =
  byteBuilder .| sinkFileBS path where
    dType = dataType header
    byteBuilder = do
      yield $ headerBuilder fType header
      mapC (matrixIntToBin dType)
