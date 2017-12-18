{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.ConduitHeader
( ConduitHeader
, conduitHeader
) where

import Conduit ((.|), ($$+), ($$++), ConduitM, ResumableSource,
                mapMC, mapM_C, sinkList, takeCE, takeWhileCE)
import Control.Monad (liftM2, when)
import Control.Monad.Except (MonadError(..))
import Data.Attoparsec.ByteString (parseOnly) 
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Conversion (FromByteString(..))
import Data.Void (Void)
import Numeric.LinearAlgebra (I)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..), compatibleHeaders)
import School.Types.DataType (fromIdxIndicator)
import School.Types.Decoding (binToInt)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))
import School.Utils.Constants (binSeparator, separator)

type ConduitHeader m =
    ConduitM () ByteString m ()
 -> m (ResumableSource m ByteString)

conduitHeader :: (LiftResult m, MonadError Error m)
              => FileType
              -> MatrixHeader
              -> ConduitHeader m
conduitHeader SM header source = do
  let sink = smConduitHeader header
  (resumable, _) <- source $$+ sink
  return resumable 
conduitHeader IDX header source =
  idxConduitHeader header source
conduitHeader _ _ _ = undefined

type HeaderSink m =
  ConduitM ByteString Void m ()

getInt :: (LiftResult m, MonadError Error m)
       => [I]
       -> m Int
getInt xs = do
  when (length xs < 1)
       (throwError "Insufficient data in IDX file")
  return . fromIntegral . head $ xs
  
getDim :: (LiftResult m)
       => ResumableSource m ByteString
       -> m (ResumableSource m ByteString, [I])
getDim resumable =
  resumable $$++ takeCE 4
         .| mapMC (liftResult . binToInt)
         .| sinkList

idxConduitHeader :: (LiftResult m, MonadError Error m)
                 => MatrixHeader
                 -> ConduitHeader m
idxConduitHeader header source = do
  (resumable, first) <- source $$+ takeCE 4
                     .| mapMC (return . (map fromEnum) . unpack)
                     .| sinkList
  when (length first < 1)
       (throwError "IDX file empty")
  let spec = head first
  let checks = length spec == 4
            && spec!!0 == 0
            && spec!!1 == 0
  when (not checks)
       (throwError "Incorrect IDX header")
  dataType <- liftResult . fromIdxIndicator $ spec!!2  
  let dims = spec!!3
  (resumable', nRows) <- getDim resumable
  rows <- getInt nRows
  let go d r acc = if d <= 1
                     then return (r, acc)
                     else do
                       (r', n) <- getDim r
                       go (d - 1) r' (liftM2 (*) n acc)
  (resumable'', nCols) <- go dims resumable' [1]
  cols <- getInt nCols
  let header' = MatrixHeader { dataType, cols, rows }
  when (not $ compatibleHeaders header header')
       (throwError $ "Expected header " ++ (show header)
                  ++ ", found " ++ (show header'))
  return resumable''

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
