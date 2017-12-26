{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.MatrixSource
( MatrixSource
, matrixDoubleSource
, poolMatrix
) where

import Conduit ((.|), ConduitM, MonadResource, mapMC,
                nullC, sourceFileBS, takeCE)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import Numeric.LinearAlgebra (Element, Matrix, R)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileHeader (FileHeader(..))
import School.Types.Decoding (binToMatrixDouble)
import School.Types.DataType (getSize)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

type MatrixSource m a = ConduitM () (Matrix a) m ()

poolMatrix :: (Element a, Monad m)
           => Int
           -> (ByteString -> m (Matrix a))
           -> ConduitM ByteString (Matrix a) m ()
poolMatrix chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop

matrixDoubleSource :: (LiftResult m, MonadError Error m, MonadResource m)
                   => FileHeader
                   -> FilePath
                   -> MatrixSource m R
matrixDoubleSource header@FileHeader { cols, rows, dataType } path =
  let size = rows * cols * getSize dataType
      source = sourceFileBS path
      cHeader = conduitHeader header
      trans = liftResult
            . (binToMatrixDouble dataType rows cols)
  in source .| cHeader .| poolMatrix size trans
