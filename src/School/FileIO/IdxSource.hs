module School.FileIO.IdxSource
( idxSource ) where

import Conduit ((.|), mapC, mapM_C, sourceFileBS, takeCE)
import Data.ByteString (unpack)
import Numeric.LinearAlgebra ((><))
import School.App.AppS (liftAppS)
import School.FileIO.AppIO (Confirmer)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource (MatrixSource, poolMatrix)
import School.Types.PosInt (getPosInt)

idxSource :: MatrixHeader
          -> FilePath
          -> MatrixSource Double          
idxSource header path = do
  let r = getPosInt . rows $ header
  let c = getPosInt . cols $ header
  let transformer = liftAppS
                  . Right
                  . (r >< c)
                  . (map fromIntegral)
                  . (map fromEnum)
                  . unpack
  sourceFileBS path .| confirm
                    .| poolMatrix (r*c) transformer

confirm :: Confirmer a
confirm = do
  takeCE 4 .| checkHeader
  mapC id

msg :: String
msg = "IDX format not compatible (NOTE: Only "
   ++ "configured for unsigned byte)"

compat :: [Int] -> Bool
compat ints = length ints == 4
           && ints!!0 == 0
           && ints!!1 == 0
           && ints!!2 == 8

checkHeader :: Confirmer a
checkHeader = mapC unpack
           .| mapM_C (\word8s -> do
                let ints = map fromEnum word8s
                let result = if (not $ compat ints)
                               then Left msg
                               else Right ()
                liftAppS result)
