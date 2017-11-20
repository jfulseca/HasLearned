{-# LANGUAGE NamedFieldPuns #-}

module HML.FileIO.MatrixSource
( ByteStringToMatrix(..)
, confirmMatrixHeader
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
      transformer = ByteStringToMatrix
        { context
        , suggestion
        , transform = runGet (toMatrixDouble r c)
        }
  in poolMatrix (r * c * doubleSize) transformer

type MatrixConduit a = ConduitM ByteString
                                (Matrix a)
                                AppIO
                                ()


data ByteStringToMatrix a = ByteStringToMatrix
  { context :: String
  , suggestion :: Maybe String
  , transform :: ByteString -> Either String (Matrix a)
  }

poolMatrix :: (Element a, Storable a)
           => Int
           -> ByteStringToMatrix a
           -> MatrixConduit a
poolMatrix chunkSize ByteStringToMatrix{context, suggestion, transform }  = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop
  transformer bytes = do
    let result = transform bytes
    liftAppIO $ errorContext context suggestion result
