{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.IntListSource
( intListSource ) where

import Conduit ((.|), ConduitM, MonadResource, mapMC,
                nullC, sourceFileBS, takeCE)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileHeader (FileHeader(..))
import School.Types.Decoding (binToListInt)
import School.Types.DataType (DataType(..), getSize)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

poolInt :: (Monad m)
           => Int
           -> (ByteString -> m [Int])
           -> ConduitM ByteString [Int] m ()
poolInt chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop

intListSource :: (LiftResult m, MonadError Error m, MonadResource m)
              => FileHeader
              -> FilePath
              -> ConduitM () [Int] m ()
intListSource header@FileHeader { dataType, rows } path =
  let size = rows * getSize dataType
      source = sourceFileBS path
      cHeader = conduitHeader header
      trans = liftResult
            . (binToListInt INT08B)
  in source .| cHeader .| poolInt size trans
