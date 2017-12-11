{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.SmSource
( confirm
, smSource
) where

import Conduit ((.|), mapC, takeCE, takeWhileCE)
import School.FileIO.Confirmer (Confirmer, confirmAtom)
import School.FileIO.MatrixHeader (MatrixHeader(..), compatibleHeaders)
import School.FileIO.MatrixSource (MatrixSource, matrixDoubleSource)
import System.FilePath (FilePath)
import School.Utils.Constants (binSeparator, separator)

smSource :: MatrixHeader
         -> FilePath
         -> MatrixSource Double
smSource = matrixDoubleSource confirm

confirm :: MatrixHeader
        -> Confirmer a
confirm header = do
  takeCE 1 .| confirmAtom (==separator)
  takeWhileCE (/= binSeparator) .| confirmAtom (compatibleHeaders header)
  takeCE 1 .| confirmAtom (==separator)
  mapC id
