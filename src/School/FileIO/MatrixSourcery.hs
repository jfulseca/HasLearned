{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.MatrixSourcery
( MatrixConduit
, MatrixSourcery
, matrixDoubleSourcery
, poolMatrix
) where

import Conduit (($$+-), (.|), ConduitM, MonadResource, mapMC,
                nullC, sourceFileBS, takeCE)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import Numeric.LinearAlgebra (Element, Matrix, R)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileType (FileType(..))
import School.FileIO.FileHeader (FileHeader(..))
import School.Types.Decoding (binToMatrixDouble)
import School.Types.DataType (getSize)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))
import School.Types.Slinky (Slinky(..))
import School.Types.Sourcery (Sourcery)
import School.Unit.CostFunction (AlterConduit)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)

type MatrixConduit m a = ConduitM ByteString
                                  (Matrix a)
                                  m
                                  ()

poolMatrix :: (Element a, Monad m)
           => Int
           -> (ByteString -> m (Matrix a))
           -> MatrixConduit m a
poolMatrix chunkSize transformer = loop where
  loop = do
    takeCE chunkSize .| mapMC transformer
    isEmpty <- nullC
    if isEmpty
      then return ()
      else loop

type MatrixSourcery m a r = AlterConduit a m
                         -> Sourcery (ForwardStack a) m r

toStack :: (Monad m)
        => ConduitM (Matrix a) (ForwardStack a) m ()
toStack = mapMC $ \matrix ->
  return ([BatchActivation matrix], SNil)

matrixDoubleSourcery :: (LiftResult m, MonadError Error m, MonadResource m)
                     => FileType
                     -> FileHeader
                     -> FilePath
                     -> MatrixSourcery m R r
matrixDoubleSourcery fType
                     header@FileHeader { cols, rows, dataType }
                     path
                     alterConduit
                     sink = do
  let size = rows * cols * getSize dataType
  let byteSource = sourceFileBS path
  cHeader <- conduitHeader fType header byteSource
  let trans = liftResult
            . (binToMatrixDouble dataType rows cols)
  cHeader $$+- alterConduit (poolMatrix size trans .| toStack)
            .| sink
