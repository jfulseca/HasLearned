{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.App.CSVReader
( csvToBinary
, csvToMatrixDouble
, parseDoubles
, readCSV
) where

import Conduit ((.|), ConduitM, mapC, mapMC, sourceFileBS)
import qualified Data.ByteString as BS
import Data.ByteString.Conversion (fromByteString)
import Data.Void (Void)
import qualified Data.Conduit.Binary as CB
import School.FileIO.AppIO (AppIO, maybeToAppIO)
import School.FileIO.FileType (FileType(..))
import School.FileIO.FileHeader (FileHeader(..))
import School.FileIO.MatrixSink (matrixDoubleSink)
import School.Utils.Constants (binComma)
import Numeric.LinearAlgebra ((><), Matrix)

parseDoubles :: [BS.ByteString] -> Maybe [Double]
parseDoubles = mapM fromByteString

readCSV :: FilePath -> ConduitM ()
                                [BS.ByteString]
                                AppIO
                                ()
readCSV path = sourceFileBS path
            .| CB.lines
            .| mapC (BS.split binComma)

csvToMatrixDouble :: FileHeader
                  -> ConduitM [BS.ByteString]
                              (Matrix Double)
                              AppIO
                              ()
csvToMatrixDouble FileHeader { cols } =
    mapC parseDoubles
 .| mapMC (maybeToAppIO "Could not parse doubles")
 .| mapC (1 >< cols)

csvToBinary :: FilePath
            -> FilePath
            -> FileHeader
            -> ConduitM () Void AppIO ()
csvToBinary inPath outPath header  =
    readCSV inPath
 .| csvToMatrixDouble header
 .| matrixDoubleSink SM header outPath
