{-# LANGUAGE TemplateHaskell #-}

module School.App.Test.CSVReader
( csvReaderTest ) where

import Conduit ((.|), runConduit, sinkList)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Data.List.Split (chunksOf)
import School.App.CSVReader
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileHeader (FileHeader(..))
import School.FileIO.MatrixSourcery (matrixDoubleSourcery)
import School.FileIO.FileType (FileType(..))
import School.Types.DataType (DataType(..))
import School.Types.Slinky (Slinky(..))
import School.Unit.UnitActivation (UnitActivation(..))
import qualified Numeric.LinearAlgebra as NL
import Prelude hiding (appendFile, writeFile)
import System.Directory (removeFile)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH

matrixConcat :: (NL.Element a)
             => [NL.Matrix a]
             -> Either String [NL.Matrix a]
matrixConcat [] = return $ []
matrixConcat matrices = do
  let nCols = NL.cols . head $ matrices
  let compat = and $ map (\m -> NL.cols m == nCols) matrices
  if (not compat) then (Left "Matrices must have same # columns")
    else do
      let allRows = concat . concat $ map NL.toLists matrices
      return $ [NL.fromLists (chunksOf nCols allRows)]

prop_convert_csv_file :: Property
prop_convert_csv_file = monadicIO $ do
  let filePath = "test/data/csvTest.csv"
  let bFileName = "test.dat"
  let header = FileHeader DBL64B 51 3
  writeRes <- run . runAppIO . runConduit $
    csvToBinary filePath bFileName header
  assert $ isRight writeRes
  readRes <- run . runAppIO . runConduit $
      readCSV filePath
     .| csvToMatrixDouble header
     .| sinkList
  let original = readRes >>= matrixConcat
  let check = fmap (fmap $ \m -> ([BatchActivation m], SNil)) original
  written <- run . runAppIO $
    matrixDoubleSourcery SM header bFileName id sinkList
  liftIO $ removeFile bFileName
  assert $ check == written

csvReaderTest :: TestTree
csvReaderTest = $(testGroupGenerator)
