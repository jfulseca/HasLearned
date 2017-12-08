{-# LANGUAGE NamedFieldPuns #-}

module School.Train.MultiLayerPerceptron
( MLPOptions(..)
, multiLayerPerceptron
) where

import Conduit ((.|), mapC)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource (matrixDoubleSource)
import School.Train.GradientDescent (gradientDescent)
import School.Types.PosInt (posInt)
import School.Types.TypeName (TypeName(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams)
import System.FilePath (FilePath)

data MLPOptions a =
  MLPOptions { hiddenDims :: [Int]
             , nBatches :: Int
             , nFeatures :: Int
             }

multiLayerPerceptron :: MLPOptions a
                     -> FilePath
                     -> Either String ( Maybe [a] 
                                      , [UnitParams a])
multiLayerPerceptron options path = do
  let nRows = posInt . nBatches $ options
  let nCols = posInt . nFeatures $ options
  case (sequence [nRows, nCols]) of
    Just [rows, cols] -> do
      let header = MatrixHeader { cols, rows, dataType = DBL }
      let source = matrixDoubleSource header path
                .| mapC BatchActivation
      let cost = undefined
      let update = undefined
      let condition = undefined
      let handler = undefined
      let initState = undefined
      endState <- gradientDescent source
                                  units
                                  cost
                                  update
                                  condition
                                  handler
                                  initState
      Left ""
    _ -> Left "Number of batches and features must be positive integers"
