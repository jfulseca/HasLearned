{-# LANGUAGE FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}

module School.FileIO.Source
( Source(..)
, pool
, source
) where

import Conduit ((.|), ConduitM, MonadResource, mapMC, nullC, sourceFileBS, takeCE)
import Control.Monad ((>=>), unless)
import Control.Monad.Except (MonadError)
import Data.ByteString (ByteString)
import Numeric.LinearAlgebra (Matrix, R)
import School.FileIO.BinConversion (binConversion)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileHeader (FileHeader(..))
import School.FileIO.FilePath (FilePath)
import School.Types.DataType (DataType(..), getSize)
import School.Types.Decoding (binToDouble, binToListInt, binToMatrixDouble)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

class Source a where
  decodeByteString :: FileHeader -> ByteString -> Either Error a

pool :: (Monad m)
     => Int
     -> (ByteString -> m a)
     -> ConduitM ByteString a m ()
pool chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    unless isEmpty loop

source :: (LiftResult m, MonadError Error m, MonadResource m, Source a)
       => FileHeader
       -> FilePath
       -> ConduitM () a m ()
source header@FileHeader { dataType, cols, rows } path =
  let size = getSize dataType * cols * rows
      byteSource = sourceFileBS path
      cHeader = conduitHeader header
      trans = liftResult . decodeByteString header
  in byteSource .| cHeader .| pool size trans

instance Source (Matrix R) where
  decodeByteString FileHeader{ dataType, cols, rows } =
    binToMatrixDouble dataType rows cols

instance Source Double where
  decodeByteString FileHeader{ dataType } =
    binConversion dataType DBL64B >=> binToDouble

instance Source [Int] where
  decodeByteString FileHeader{ dataType } = binToListInt dataType
