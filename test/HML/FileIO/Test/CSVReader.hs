{-# LANGUAGE TemplateHaskell #-}

module HML.FileIO.Test.CSVReader
( csvReaderTest ) where

import Conduit ((.|), ConduitM, sinkList)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Double.Conversion.Text (toExponential)
import Data.Either (isRight)
import Data.List.Split (chunksOf)
import Data.Text (append, intercalate, singleton)
import Data.Text.IO (appendFile, writeFile)
import Data.Void (Void)
import HML.FileIO.CSVReader
import HML.FileIO.AppIO (AppIO, runAppIO)
import HML.FileIO.MatrixHeader (MatrixHeader(..))
import HML.FileIO.MatrixSource (matrixDoubleSource)
import HML.Types.FloatEq (FloatEq(..))
import HML.Types.TypeName (TypeName(..))
import qualified Numeric.LinearAlgebra as NL
import Prelude hiding (appendFile, writeFile)
import System.Directory (removeFile)
import System.Random (getStdRandom, random)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pre, run)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH

testRun :: ConduitM () Void AppIO a
        -> PropertyM IO (Either String a)
testRun = run . runAppIO

prop_convert_csv :: [Double] -> Positive Int -> Property
prop_convert_csv list nRows@(Positive r) = monadicIO $ do
  let c = length list
  pre $ c > 0
  tag <- liftIO (getStdRandom random :: IO Int)
  let nCols = Positive c
  let fileName = "test" ++ (show tag) ++ ".csv"
  let bFileName = "test" ++ (show tag) ++ ".dat"
  let ex = toExponential 15
  let sep = singleton ','
  let endl = singleton '\n'
  let row = intercalate sep (map ex list)
  liftIO $ writeFile fileName row
  liftIO $ replicateM_ (r - 1)
                       (appendFile fileName (append endl row))
  let header = MatrixHeader DBL nRows nCols
  writeRes <- testRun $ csvToBinary fileName
                                    bFileName
                                    header
  assert $ isRight writeRes
  readRes <- testRun $ matrixDoubleSource header bFileName .| sinkList
  let mList = concat . replicate r $ list
  let check = Right [(r NL.>< c) mList] :: Either String [NL.Matrix Double]
  liftIO $ removeFile fileName
  liftIO $ removeFile bFileName
  assert $ readRes ~= check

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
  let filePath = "src/HML/Test/data/csvTest.csv"
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
