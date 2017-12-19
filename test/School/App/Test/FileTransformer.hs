{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.App.Test.FileTransformer
( fileTransformerTest ) where

import Conduit (await, liftIO)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (liftA2)
import Data.Default.Class (def)
import Data.Function (on)
import Numeric.LinearAlgebra ((?), Matrix, R)
import School.App.FileTransformer
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileApp (fileApp)
import School.FileIO.FileType (FileType(..), toExtension)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSourcery (matrixDoubleSourcery)
import School.TestUtils (assertRight, dummyMatrix)
import School.Types.DataType (DataType(..))
import School.Types.Error (Error)
import School.Utils.Either (isLeft, isRight)
import System.Directory (removeFile)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run)

inFileName :: FileType -> FilePath
inFileName fType = "test/data/matrix3x3."
                ++ toExtension fType

header3x3 :: MatrixHeader
header3x3 = MatrixHeader { dataType = DBL64B
                         , cols = 3
                         , rows = 3
                         }

testFile :: FileType -> String
testFile fType = "test." ++ toExtension fType

fileEq :: FilePath -> FilePath -> IO (Bool)
fileEq = liftA2 (==) `on` BL.readFile

readMat :: FileType
        -> MatrixHeader
        -> FilePath
        -> PropertyM IO (Either Error
                                (Maybe (Matrix R)))
readMat fType header path = run . runAppIO $
 matrixDoubleSourcery fType header path await

prop_copy :: FileType -> Property
prop_copy fType = monadicIO $ do
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just fType
                    , outFileOpt = testFile fType
                    , outFileTypeOpt = Just fType
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isRight result
  equal <- liftIO $ fileEq (inFileName fType) (testFile fType)
  assert equal

prop_copy_guess_filetypes :: FileType -> Property
prop_copy_guess_filetypes fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , outFileOpt = (testFile fType)
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  equal <- liftIO $ fileEq (inFileName fType) (testFile fType)
  liftIO $ removeFile (testFile fType)
  assert equal

prop_copy_add_extension :: FileType -> Property
prop_copy_add_extension fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just fType
                    , outFileOpt = "test"
                    , outFileTypeOpt = Just fType
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isRight result
  equal <- liftIO $ fileEq (inFileName fType) (testFile fType)
  liftIO $ removeFile (testFile fType)
  assert equal

prop_skip_rows :: FileType -> Property
prop_skip_rows fType = monadicIO $ do
  let outHeader = header3x3 { rows = 1 }
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just fType
                    , outFileOpt = (testFile fType)
                    , outFileTypeOpt = Just fType
                    , skipRowsOpt = Just 2
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [2]
  readRes <- readMat fType outHeader (testFile fType)
  liftIO $ removeFile (testFile fType)
  assertRight (== check) readRes

prop_skip_too_many_rows :: FileType -> Property
prop_skip_too_many_rows fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just fType
                    , skipRowsOpt = Just 4
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isLeft result

prop_limit_extent :: FileType -> Property
prop_limit_extent fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just fType
                    , nRowsOpt = Just 2
                    , outFileOpt = (testFile fType)
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [0, 1]
  let outHeader = header3x3 { rows = 2 }
  readRes <- readMat fType outHeader (testFile fType)
  liftIO $ removeFile (testFile fType)
  assertRight (== check) readRes

prop_skip_and_limit :: FileType -> Property
prop_skip_and_limit fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just fType
                    , nRowsOpt = Just 1
                    , skipRowsOpt = Just 1
                    , outFileOpt = (testFile fType)
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [1]
  let outHeader = header3x3 { rows = 1 }
  readRes <- readMat fType outHeader (testFile fType)
  liftIO $ removeFile (testFile fType)
  assertRight (== check) readRes

prop_limit_too_many_rows :: FileType -> Property
prop_limit_too_many_rows fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just fType
                    , nRowsOpt = Just 4
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isLeft result

prop_limit_and_skip_too_many :: FileType -> Property
prop_limit_and_skip_too_many fType = monadicIO $ do
  let options = def { inFileOpt = (inFileName fType)
                    , inFileTypeOpt = Just SM
                    , nRowsOpt = Just 3
                    , skipRowsOpt = Just 1
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isLeft result

fileTransformerTest :: TestTree
fileTransformerTest = $(testGroupGenerator)
