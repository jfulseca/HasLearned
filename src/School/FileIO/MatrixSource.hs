{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.MatrixSource
( MatrixSource
, confirmMatrixHeader
, matrixDoubleSource
, poolMatrix
, poolMatrixDouble
) where

import Conduit ((.|), ConduitM, mapC, mapMC, nullC,
                sourceFileBS, takeCE, takeWhileCE)
import Data.ByteString (ByteString)
import School.App.AppS (AppS, liftAppS, throwConduit)
import School.FileIO.AppIO (Confirmer, confirmAtom)
import School.FileIO.MatrixHeader (MatrixHeader(..), compatibleHeaders)
import School.Types.DoubleConversion (toMatrixDouble)
import School.Types.PosInt (PosInt, getPosInt)
import School.Types.TypeName (TypeName(..), getSize)
import Numeric.LinearAlgebra (Element, Matrix)
import System.FilePath (FilePath)
import School.Utils.Constants (binSeparator, separator)

type MatrixSource a =
  ConduitM () (Matrix a) (AppS a) ()

matrixDoubleSource :: MatrixHeader
                   -> FilePath
                   -> MatrixSource Double
matrixDoubleSource header path = do
  let r = rows header
  let c = cols header
  let dType = dataType header
  case dType of
    DBL64B -> sourceFileBS path
        .| confirmMatrixHeader header
        .| poolMatrixDouble r c
    _ -> throwConduit $ "Expected data type \'DBL\', got \'" ++ (show dType) ++ "\'"

confirmMatrixHeader :: MatrixHeader
                    -> Confirmer a
confirmMatrixHeader header = do
  takeCE 1 .| confirmAtom (==separator)
  takeWhileCE (/= binSeparator) .| confirmAtom (compatibleHeaders header)
  takeCE 1 .| confirmAtom (==separator)
  mapC id

poolMatrixDouble :: PosInt
                 -> PosInt
                 -> MatrixConduit Double
poolMatrixDouble nRows nCols =
  let r = getPosInt nRows
      c = getPosInt nCols
      s = getSize DBL64B
      transformer = liftAppS
                  . (toMatrixDouble DBL64B r c)
  in poolMatrix (r * c * s) transformer

type MatrixConduit a = ConduitM ByteString
                                (Matrix a)
                                (AppS a)
                                ()

poolMatrix :: (Element a)
           => Int
           -> (ByteString -> AppS a (Matrix a))
           -> MatrixConduit a
poolMatrix chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop
