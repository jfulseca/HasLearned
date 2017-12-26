{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.DoubleSource
( doubleSource ) where

import Conduit ((.|), ConduitM, MonadResource, mapMC,
                nullC, sourceFileBS, takeCE)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import School.FileIO.BinConversion (binConversion)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileHeader (FileHeader(..))
import School.Types.Decoding (binToDouble)
import School.Types.DataType (DataType(..), getSize)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

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

doubleSource :: (LiftResult m, MonadError Error m, MonadResource m)
             => FileHeader
             -> FilePath
             -> ConduitM () Double m ()
doubleSource header@FileHeader { dataType } path =
  let size = getSize dataType
      source = sourceFileBS path
      cHeader = conduitHeader header
      trans = liftResult
            . (\b -> binConversion dataType DBL64B b >>= binToDouble)
  in source .| cHeader .| poolDouble size trans
