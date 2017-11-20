module HML.FileIO.MatrixSink
( matrixDoubleSink ) where

import Conduit ((.|), ConduitM, mapC, sinkFileBS, yield)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.Serialize.Put (runPut)
import Data.Void (Void)
import HML.FileIO.AppIO (AppIO)
import HML.FileIO.MatrixHeader (MatrixHeader(..))
import HML.Types.DoubleToBinary (doubleToBinary)
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
  mapM_ doubleToBinary (concat . toLists $ matrix)
