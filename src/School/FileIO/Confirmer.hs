{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.Confirmer
( Confirmer
, confirmAtom
, confirmer
, idxConfirm
, smConfirm
) where

import Conduit ((.|), ConduitM, mapC, mapCE, mapM_C, takeCE, takeWhileCE)
import Control.Monad (when)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Conversion (FromByteString, parser)
import School.App.AppS (AppS, liftAppS, throw)
import School.FileIO.MatrixHeader (MatrixHeader(..), compatibleHeaders)
import School.FileIO.FileType (FileType(..))
import School.Types.DataType (DataType, fromIdxIndicator)
import School.Utils.Constants (separator, binSeparator)

type Confirmer a = ConduitM ByteString
                            ByteString
                            (AppS a)
                            ()

confirmAtom :: (Eq b, FromByteString b, Show b)
            => (b -> Bool)
            -> Confirmer a
confirmAtom check = mapM_C $ \bytes -> do
  let parseResult = atomParser check bytes
  liftAppS parseResult
  where
    atomParser c b = do
      atom <- parseOnly parser b
      if c atom
        then Right ()
        else Left $ msg b
    msg b = "Parser gave unexpected "
         ++ "result " ++ (show b)

smConfirm :: MatrixHeader
        -> Confirmer a
smConfirm header = do
  takeCE 1 .| confirmAtom (==separator)
  takeWhileCE (/= binSeparator) .| confirmAtom (compatibleHeaders header)
  takeCE 1 .| confirmAtom (==separator)
  mapC id

idxConfirm :: MatrixHeader -> Confirmer a
idxConfirm MatrixHeader { dataType } = do
  takeCE 4 .| checkHeader dataType
  mapC id

errorMsg :: Int -> DataType -> String
errorMsg i t = "Type indicator " ++ (show i)
            ++ " does not correspond to " ++ (show t)

compat :: DataType -> [Int] -> AppS a ()
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
                            else Left $ errorMsg coeff dType)
                    dType'

checkHeader :: DataType -> Confirmer a
checkHeader dType =
    mapC unpack
 .| mapCE fromEnum
 .| mapM_C (compat dType)

confirmer :: FileType
          -> MatrixHeader
          -> Confirmer a
confirmer IDX = idxConfirm
confirmer SM = smConfirm
confirmer _ = undefined
