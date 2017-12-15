{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.App.Test.FileTransformer
( fileTransformerTest ) where

import Conduit (await, liftIO)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (liftA2)
import Data.Default.Class (def)
import Data.Function (on)
import Numeric.LinearAlgebra ((?))
import School.App.AppS (runAppSPure)
import School.App.FileTransformer
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileApp (fileApp)
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSourcery (matrixDoubleSourcery)
import School.TestUtils (assertRight, dummyMatrix)
import School.Types.DataType (DataType(..))
import School.Utils.Either (isLeft, isRight)
import System.Directory (removeFile)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO, run)

sm3x3File :: String
sm3x3File = "test/data/matrix3x3.sm"

header3x3 :: MatrixHeader
header3x3 = MatrixHeader { dataType = DBL64B
                         , cols = 3
                         , rows = 3
                         }

testFile :: String
testFile = "test.sm"

fileEq :: FilePath -> FilePath -> IO (Bool)
fileEq = liftA2 (==) `on` BL.readFile

prop_copy :: Property
prop_copy = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , outFileOpt = testFile
                    , outFileTypeOpt = Just SM
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_copy_guess_filetypes :: Property
prop_copy_guess_filetypes = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , outFileOpt = testFile
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_copy_add_extension :: Property
prop_copy_add_extension = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , outFileOpt = "test"
                    , outFileTypeOpt = Just SM
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_skip_rows :: Property
prop_skip_rows = monadicIO $ do
  let outHeader = header3x3 { rows = 1 }
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , outFileOpt = testFile
                    , outFileTypeOpt = Just SM
                    , skipRowsOpt = Just 2
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [2]
  readRes <- run . runAppSPure $
    matrixDoubleSourcery SM outHeader testFile await
  liftIO $ removeFile testFile
  assertRight (== check) readRes

prop_skip_too_many_rows :: Property
prop_skip_too_many_rows = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , skipRowsOpt = Just 4
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isLeft result

prop_limit_extent :: Property
prop_limit_extent = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , nRowsOpt = Just 2
                    , outFileOpt = testFile
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [0, 1]
  let outHeader = header3x3 { rows = 2 }
  readRes <- run . runAppSPure $
    matrixDoubleSourcery SM outHeader testFile await
  liftIO $ removeFile testFile
  assertRight (== check) readRes

prop_skip_and_limit :: Property
prop_skip_and_limit = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , nRowsOpt = Just 1
                    , skipRowsOpt = Just 1
                    , outFileOpt = testFile
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [1]
  let outHeader = header3x3 { rows = 1 }
  readRes <- run . runAppSPure $
    matrixDoubleSourcery SM outHeader testFile await
  liftIO $ removeFile testFile
  assertRight (== check) readRes

prop_limit_too_many_rows :: Property
prop_limit_too_many_rows = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , nRowsOpt = Just 4
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isLeft result

prop_limit_and_skip_too_many :: Property
prop_limit_and_skip_too_many = monadicIO $ do
  let options = def { inFileOpt = sm3x3File
                    , inFileTypeOpt = Just SM
                    , nRowsOpt = Just 3
                    , skipRowsOpt = Just 1
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isLeft result

fileTransformerTest :: TestTree
fileTransformerTest = $(testGroupGenerator)
