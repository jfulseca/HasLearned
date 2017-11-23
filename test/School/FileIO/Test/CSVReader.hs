{-# LANGUAGE TemplateHaskell #-}

module School.FileIO.Test.CSVReader
( csvReaderTest ) where

import Conduit ((.|), sinkList)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Data.List.Split (chunksOf)
import School.FileIO.CSVReader
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource (matrixDoubleSource)
import School.TestUtils (testRun)
import School.Types.FloatEq (FloatEq(..))
import School.Types.TypeName (TypeName(..))
import qualified Numeric.LinearAlgebra as NL
import Prelude hiding (appendFile, writeFile)
import System.Directory (removeFile)
import Test.QuickCheck.Monadic (assert, monadicIO)
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
  let header = MatrixHeader DBL (Positive 51) (Positive 3)
  writeRes <- testRun $ csvToBinary filePath
                                    bFileName
                                    header
  assert $ isRight writeRes
  readRes <- testRun $ readCSV filePath
                    .| csvToMatrixDouble header
                    .| sinkList
  let original = readRes >>= matrixConcat
  written <- testRun $ matrixDoubleSource header bFileName
                    .| sinkList
  liftIO $ removeFile bFileName
  assert $ original ~= written

csvReaderTest :: TestTree
csvReaderTest = $(testGroupGenerator)