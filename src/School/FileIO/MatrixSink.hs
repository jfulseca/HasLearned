module School.FileIO.MatrixSink
( MatrixSink
, matrixDoubleSink
) where

import Conduit ((.|), ConduitM, mapC, sinkFileBS, yield)
import Data.ByteString (ByteString)
import Data.Void (Void)
import School.App.AppS (AppS)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.DoubleConversion (fromMatrixDouble)
import Numeric.LinearAlgebra.Data (Matrix)

type MatrixSink a = ConduitM (Matrix a)
                             Void
                             (AppS a)
                             ()

matrixDoubleSink :: (MatrixHeader -> ByteString)
                 -> MatrixHeader
                 -> FilePath
                 -> MatrixSink Double
matrixDoubleSink headerBuild header path =
  byteBuilder .| sinkFileBS path where
    dType = dataType header
    byteBuilder = do
      yield $ headerBuild header
      mapC (fromMatrixDouble dType)
