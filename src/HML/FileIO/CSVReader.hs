{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module HML.FileIO.CSVReader
( csvToBinary
, csvToMatrixDouble
, parseDoubles
, readCSV
) where

import Conduit ((.|), ConduitM, mapC, mapMC, sourceFileBS)
import qualified Data.ByteString as BS
import Data.ByteString.Conversion (fromByteString)
import qualified Data.Conduit.Binary as CB
import HML.FileIO.AppIO (AppIO, ConduitAppIO, maybeToAppIO)
import HML.FileIO.MatrixHeader (MatrixHeader(..))
import HML.FileIO.MatrixSink (matrixDoubleSink)
import HML.Types.PosInt (getPosInt)
import HML.Utils.Constants (binComma)
import Numeric.LinearAlgebra ((><), Matrix)

parseDoubles :: [BS.ByteString] -> Maybe [Double]
parseDoubles = sequence . map fromByteString

readCSV :: FilePath -> ConduitM ()
                                [BS.ByteString]
                                AppIO
                                ()
readCSV path = sourceFileBS path
            .| CB.lines
            .| mapC (BS.split binComma)

csvToMatrixDouble :: MatrixHeader
                  -> ConduitM [BS.ByteString]
                              (Matrix Double)
                              AppIO
                              ()
csvToMatrixDouble MatrixHeader { cols } =
    mapC parseDoubles
 .| mapMC (maybeToAppIO "Could not parse doubles")
 .| mapC (1 >< (getPosInt cols))

csvToBinary :: FilePath
            -> FilePath
            -> MatrixHeader
            -> ConduitAppIO 
csvToBinary inPath outPath header  = 
    readCSV inPath
 .| csvToMatrixDouble header 
 .| matrixDoubleSink header outPath
