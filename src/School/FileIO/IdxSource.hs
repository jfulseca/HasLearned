{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.IdxSource
( idxSource ) where

import Conduit ((.|), mapC, mapCE, mapM_C, takeCE)
import Control.Monad (when)
import Data.ByteString (unpack)
import School.App.AppS (AppS, liftAppS, throw)
import School.FileIO.Confirmer (Confirmer)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource (MatrixSource, matrixDoubleSource)
import School.Types.TypeName (TypeName(..), fromIdxIndicator)

idxSource :: MatrixHeader
          -> FilePath
          -> MatrixSource Double
idxSource = matrixDoubleSource confirm

confirm :: MatrixHeader -> Confirmer a
confirm MatrixHeader { dataType } = do
  takeCE 4 .| checkHeader dataType
  mapC id

msg :: Int -> TypeName -> String
msg i t = "Type indicator " ++ (show i)
       ++ " does not correspond to " ++ (show t)

compat :: TypeName -> [Int] -> AppS a ()
compat dType ints = do
  let check = length ints == 4
           && ints!!0 == 0
           && ints!!1 == 0
  when (not check)
       (throw "Invalid IDX format")
  let coeff = ints!!2
  let dType' = fromIdxIndicator coeff
  liftAppS $ either Left
                    (\t -> if t == dType
                            then Right ()
                            else Left $ msg coeff dType)
                    dType'

checkHeader :: TypeName -> Confirmer a
checkHeader dType =
    mapC unpack
 .| mapCE fromEnum
 .| mapM_C (compat dType)
