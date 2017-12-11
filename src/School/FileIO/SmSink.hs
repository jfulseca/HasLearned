module School.FileIO.SmSink
( smSink ) where

import Data.ByteString.Conversion (toByteString')
import School.FileIO.MatrixHeader (MatrixHeader)
import School.FileIO.MatrixSink (MatrixSink, matrixDoubleSink)
import System.FilePath (FilePath)

smSink :: MatrixHeader
       -> FilePath
       -> MatrixSink Double
smSink = matrixDoubleSink toByteString'
