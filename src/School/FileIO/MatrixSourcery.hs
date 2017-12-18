{-# LANGUAGE NamedFieldPuns #-}

module School.FileIO.MatrixSourcery
( MatrixConduit
, MatrixSourcery
, matrixDoubleSourcery
, poolMatrix
) where

import Conduit (($$+-), (.|), ConduitM, MonadResource, mapMC,
                nullC, sourceFileBS, takeCE)
import Data.ByteString (ByteString)
import School.FileIO.ConduitHeader (conduitHeader)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.Decoding (binToMatrixDouble)
import School.Types.DataType (getSize)
import School.Types.LiftResult (LiftResult(..))
import School.Types.Sourcery (Sourcery)
import Numeric.LinearAlgebra (Element, Matrix, R)

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

type MatrixSourcery m a r = Sourcery (Matrix a)
                                     m
                                     r

matrixDoubleSourcery :: (LiftResult m, MonadResource m)
                     => FileType
                     -> MatrixHeader
                     -> FilePath
                     -> MatrixSourcery m R r
matrixDoubleSourcery fType
                     header@MatrixHeader { cols, rows, dataType }
                     path
                     sink = do
  let size = rows * cols * getSize dataType
  let source = sourceFileBS path
  cHeader <- conduitHeader fType header source
  let trans = liftResult
            . (binToMatrixDouble dataType rows cols)
  cHeader $$+- (poolMatrix size trans) .| sink
