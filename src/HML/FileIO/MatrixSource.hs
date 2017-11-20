{-# LANGUAGE NamedFieldPuns #-}

module HML.FileIO.MatrixSource
( confirmMatrixHeader
, matrixDoubleSource
, poolMatrix
, poolMatrixDouble
) where

import Conduit ((.|), ConduitM, mapC, mapMC, nullC,
                sourceFileBS, takeCE, takeWhileCE)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)
import Foreign.Storable (Storable)
import HML.FileIO.AppIO (AppIO, Confirmer, appIOFail,
                         confirmAtom, liftAppIO)
import HML.FileIO.MatrixHeader (MatrixHeader(..), compatibleHeaders)
import HML.Types.DoubleToBinary (doubleFromBinary)
import HML.Types.Errors (errorContext)
import HML.Types.PosInt (PosInt, getPosInt)
import HML.Types.TypeName (TypeName(..))
import Numeric.LinearAlgebra (Element)
import Numeric.LinearAlgebra.Data ((><), Matrix)
import System.FilePath (FilePath)
import HML.Utils.Constants (binSeparator, doubleSize, separator)

matrixDoubleSource :: MatrixHeader
                   -> FilePath
                   -> ConduitM ()
                               (Matrix Double)
                               AppIO
                               ()
matrixDoubleSource header path = do
  let r = rows header
  let c = cols header
  let dType = dataType header
  case dType of
    DBL -> sourceFileBS path
        .| confirmMatrixHeader header
        .| poolMatrixDouble r c
    _ -> appIOFail $ "Expected data type \'DBL\', got \'" ++ (show dType) ++ "\'"

confirmMatrixHeader :: MatrixHeader -> Confirmer
confirmMatrixHeader header = do
  takeCE 1 .| confirmAtom (==separator)
  takeWhileCE (/= binSeparator) .| confirmAtom (compatibleHeaders header)
  takeCE 1 .| confirmAtom (==separator)
  mapC id

toMatrixDouble :: Int -> Int -> Get (Matrix Double)
toMatrixDouble nRows nCols = do
  let nElements = nRows * nCols
  list <- replicateM (nElements) doubleFromBinary
  return $ (nRows >< nCols) list

poolMatrixDouble :: PosInt
                 -> PosInt
                 -> MatrixConduit Double
poolMatrixDouble nRows nCols =
  let r = getPosInt nRows
      c = getPosInt nCols
      context = "Parsing matrix"
      suggestion = Just "Does the data have the right shape?"
      transformer bytes = do
        let result = runGet (toMatrixDouble r c) bytes
        liftAppIO $ errorContext context suggestion result
  in poolMatrix (r * c * doubleSize) transformer

type MatrixConduit a = ConduitM ByteString
                                (Matrix a)
                                AppIO
                                ()

poolMatrix :: (Element a, Storable a)
           => Int
           -> (ByteString -> AppIO (Matrix a))
           -> MatrixConduit a
poolMatrix chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop
