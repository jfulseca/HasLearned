{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.DoubleSourcery
( doubleSourcery ) where

import Conduit (($$+-), (.|), ConduitM, MonadResource, mapMC,
                nullC, sourceFileBS, takeCE)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import School.FileIO.BinConversion (binConversion)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.Decoding (binToDouble)
import School.Types.DataType (DataType(..), getSize)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))
import School.Types.Sourcery (Sourcery)

poolDouble :: (Monad m)
           => Int
           -> (ByteString -> m Double)
           -> ConduitM ByteString Double m ()
poolDouble chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop

doubleSourcery :: (LiftResult m, MonadError Error m, MonadResource m)
               => FileType
               -> MatrixHeader
               -> FilePath
               -> Sourcery Double m r
doubleSourcery fType header@MatrixHeader { dataType } path sink = do
  let size = getSize dataType
  let source = sourceFileBS path
  cHeader <- conduitHeader fType header source
  let trans = liftResult
            . (\b -> binConversion dataType DBL64B b >>= binToDouble)
  cHeader $$+- (poolDouble size trans) .| sink
