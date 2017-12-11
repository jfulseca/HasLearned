{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.SmSource
( confirmMatrixHeader
, poolMatrixDouble
, smSource
) where

import Conduit ((.|), mapC, sourceFileBS, takeCE, takeWhileCE)
import School.App.AppS (liftAppS, throwConduit)
import School.FileIO.AppIO (Confirmer, confirmAtom)
import School.FileIO.MatrixHeader (MatrixHeader(..), compatibleHeaders)
import School.FileIO.MatrixSource (MatrixConduit, MatrixSource, poolMatrix)
import School.Types.DoubleConversion (toMatrixDouble)
import School.Types.PosInt (PosInt, getPosInt)
import School.Types.TypeName (TypeName(..), getSize)
import System.FilePath (FilePath)
import School.Utils.Constants (binSeparator, separator)

smSource :: MatrixHeader
         -> FilePath
         -> MatrixSource Double
smSource header path = do
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
