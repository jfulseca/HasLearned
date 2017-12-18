module School.FileIO.ConduitHeader
( ConduitHeader
, conduitHeader
) where

import Conduit ((.|), ($$+), ConduitM, ResumableSource,
                mapM_C, takeCE, takeWhileCE)
import Data.Attoparsec.ByteString (parseOnly) 
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (FromByteString(..))
import Data.Void (Void)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader, compatibleHeaders)
import School.Types.LiftResult (LiftResult(..))
import School.Utils.Constants (binSeparator, separator)

type ConduitHeader m =
    ConduitM () ByteString m ()
 -> m (ResumableSource m ByteString)

conduitHeader :: (LiftResult m)
              => FileType
              -> MatrixHeader
              -> ConduitHeader m
conduitHeader SM header source = do
  let sink = smConduitHeader header
  (resumable, _) <- source $$+ sink
  return resumable 
conduitHeader _ _ _ = undefined

type HeaderSink m =
  ConduitM ByteString Void m ()

{-idxConduitHeader :: MatrixHeader
                 -> ConduitHeader m
idxConduitHeader header = do
-}
smConduitHeader :: (LiftResult m)
                => MatrixHeader
                -> HeaderSink m
smConduitHeader header = do
  let sepEq = (==separator)
  let compat = compatibleHeaders header
  takeCE 1 .| confirmAtom sepEq
  takeWhileCE (/= binSeparator) .| confirmAtom compat
  takeCE 1 .| confirmAtom sepEq

confirmAtom :: (Eq b, FromByteString b, LiftResult m, Show b)
            => (b -> Bool)
            -> HeaderSink m
confirmAtom check = mapM_C $ \bytes -> do
  let parseResult = atomParser check bytes
  liftResult parseResult
  where
    atomParser c b = do
      atom <- parseOnly parser b
      if c atom
        then Right ()
        else Left $ msg b
    msg b = "Parser gave unexpected "
         ++ "result " ++ (show b)

{-
idxConfirm :: MatrixHeader -> ConduitBS a
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
  liftResult $ either Left
                    (\t -> if t == dType
                            then Right ()
                            else Left $ errorMsg coeff dType)
                    dType'

checkHeader :: DataType -> ConduitBS a
checkHeader dType =
    mapC unpack
 .| mapCE fromEnum
 .| mapM_C (compat dType)

-}
