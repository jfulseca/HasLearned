module School.FileIO.MatrixSink
( matrixDoubleSink ) where

import Conduit ((.|), ConduitM, mapC, sinkFileBS, yield)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.Serialize.Put (runPut)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.DoubleConversion (putDouble)
import Numeric.LinearAlgebra.Data (Matrix, toLists)

matrixDoubleSink :: MatrixHeader
                 -> FilePath
                 -> ConduitM (Matrix Double)
                             Void
                             AppIO
                             ()
matrixDoubleSink header path =
  byteBuilder .| sinkFileBS path where
    byteBuilder = do
      yield $ toByteString' header
      mapC fromMatrixDouble

fromMatrixDouble :: Matrix Double -> ByteString
fromMatrixDouble matrix = runPut $
  mapM_ putDouble (concat . toLists $ matrix)
