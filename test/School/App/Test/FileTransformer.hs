{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.App.Test.FileTransformer
( fileTransformerTest ) where

import Conduit ((.|), await, liftIO, runConduit, sinkList)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (liftA2)
import Data.Default.Class (def)
import Data.Function (on)
import Numeric.LinearAlgebra ((?), Matrix, R)
import School.App.FileTransformer
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileApp (fileApp)
import School.FileIO.FileType (FileType(..), toExtension)
import School.FileIO.FileHeader (FileHeader(..))
import School.FileIO.Source (source)
import School.TestUtils (assertRight, dummyMatrix)
import School.Types.DataType (DataType(..))
import School.Types.Error (Error)
import School.Utils.Either (isLeft, isRight)
import System.Directory (removeFile)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run)

inFileName :: FileType -> FilePath
inFileName fType = "test/data/matrix3x3."
                ++ toExtension fType

inFileNameTwice :: DataType -> FileType -> FilePath
inFileNameTwice DBL64B fType = "test/data/matrix3x3Twice."
                            ++ toExtension fType
inFileNameTwice INT32B fType = "test/data/matrix3x3Twice_INT32B."
                            ++ toExtension fType
inFileNameTwice INT08B fType = "test/data/matrix3x3Twice_INT08B."
                            ++ toExtension fType

header3x3 :: FileHeader
header3x3 = FileHeader { dataType = DBL64B
                         , cols = 3
                         , rows = 3
                         }

testFile :: FileType -> String
testFile fType = "test." ++ toExtension fType

fileEq :: FilePath -> FilePath -> IO Bool
fileEq = liftA2 (==) `on` BL.readFile

readMat :: FileHeader
        -> FilePath
        -> PropertyM IO (Either Error
                                (Maybe (Matrix R)))
readMat header path = run . runAppIO . runConduit $
  source header path .| await

readDoubleList :: FileHeader
               -> FilePath
               -> PropertyM IO (Either Error
                                       [Double])
readDoubleList header path = run . runAppIO . runConduit $
  source header path .| sinkList

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
  liftIO $ removeFile (testFile fType)
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
  let options = def { inFileOpt = inFileName fType
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
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just fType
                    , outFileOpt = testFile fType
                    , outFileTypeOpt = Just fType
                    , skipRowsOpt = Just 2
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [2]
  readRes <- readMat outHeader (testFile fType)
  liftIO $ removeFile (testFile fType)
  assertRight (== check) readRes

prop_skip_too_many_rows :: FileType -> Property
prop_skip_too_many_rows fType = monadicIO $ do
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just fType
                    , skipRowsOpt = Just 4
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isLeft result

prop_limit_extent :: FileType -> Property
prop_limit_extent fType = monadicIO $ do
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just fType
                    , nRowsOpt = Just 2
                    , outFileOpt = testFile fType
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [0, 1]
  let outHeader = header3x3 { rows = 2 }
  readRes <- readMat outHeader (testFile fType)
  liftIO $ removeFile (testFile fType)
  assertRight (== check) readRes

prop_skip_and_limit :: FileType -> Property
prop_skip_and_limit fType = monadicIO $ do
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just fType
                    , nRowsOpt = Just 1
                    , skipRowsOpt = Just 1
                    , outFileOpt = testFile fType
                    }
  result <-liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let check = Just $ dummyMatrix 3 3 ? [1]
  let outHeader = header3x3 { rows = 1 }
  readRes <- readMat outHeader (testFile fType)
  liftIO $ removeFile (testFile fType)
  assertRight (== check) readRes

prop_limit_too_many_rows :: FileType -> Property
prop_limit_too_many_rows fType = monadicIO $ do
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just fType
                    , nRowsOpt = Just 4
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isLeft result

prop_limit_and_skip_too_many :: FileType -> Property
prop_limit_and_skip_too_many fType = monadicIO $ do
  let options = def { inFileOpt = inFileName fType
                    , inFileTypeOpt = Just SM
                    , nRowsOpt = Just 3
                    , skipRowsOpt = Just 1
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isLeft result

inFileType :: FileType -> DataType -> FilePath
inFileType fType DBL64B = "test/data/matrix3x3."
                       ++ toExtension fType
inFileType fType INT32B = "test/data/matrix3x3_INT32B."
                       ++ toExtension fType
inFileType fType INT08B = "test/data/matrix3x3_INT08B."
                       ++ toExtension fType

legalConversion :: DataType -> DataType -> Bool
legalConversion INT08B _ = True
legalConversion INT32B INT32B = True
legalConversion INT32B DBL64B = True
legalConversion DBL64B DBL64B = True
legalConversion _ _ = False

prop_convert_data_type :: FileType -> DataType -> DataType -> Property
prop_convert_data_type fType tIn tOut = monadicIO $ do
  let options = def { inFileOpt = inFileType fType tIn
                    , inDataTypeOpt = tIn
                    , inFileTypeOpt = Just fType
                    , outDataTypeOpt = Just tOut
                    , outFileOpt = testFile fType
                    , outFileTypeOpt = Just fType
                    }
  result <- liftIO . runAppIO $ fileApp options
  if legalConversion tIn tOut
    then do
      assert $ isRight result
      let inHeader = header3x3 { dataType = tIn }
      let outHeader = header3x3 { dataType = tOut }
      inRes <- readDoubleList inHeader (inFileType fType tIn)
      outRes <- readDoubleList outHeader (testFile fType)
      liftIO $ removeFile (testFile fType)
      assert $ inRes == outRes
    else assert $ isLeft result

prop_convert_file_type :: FileType -> FileType -> DataType -> Property
prop_convert_file_type fTypeIn fTypeOut dType = monadicIO $ do
  let options = def { inFileOpt = inFileType fTypeIn dType
                    , inDataTypeOpt = dType
                    , inFileTypeOpt = Just fTypeIn
                    , outFileOpt = testFile fTypeOut
                    , outFileTypeOpt = Just fTypeOut
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let header = header3x3 { dataType = dType }
  inRes <- readDoubleList header (inFileType fTypeIn dType)
  outRes <- readDoubleList header (testFile fTypeOut)
  liftIO $ removeFile (testFile fTypeOut)
  assert $ inRes == outRes

prop_convert_both_types :: FileType -> FileType -> DataType -> DataType -> Property
prop_convert_both_types fTypeIn fTypeOut tIn tOut = monadicIO $ do
  let options = def { inFileOpt = inFileType fTypeIn tIn
                    , inDataTypeOpt = tIn
                    , inFileTypeOpt = Just fTypeIn
                    , outDataTypeOpt = Just tOut
                    , outFileOpt = testFile fTypeOut
                    , outFileTypeOpt = Just fTypeOut
                    }
  result <- liftIO . runAppIO $ fileApp options
  if legalConversion tIn tOut
    then do
      assert $ isRight result
      let inHeader = header3x3 { dataType = tIn }
      let outHeader = header3x3 { dataType = tOut }
      inRes <- readDoubleList inHeader (inFileType fTypeIn tIn)
      outRes <- readDoubleList outHeader (testFile fTypeOut)
      liftIO $ removeFile (testFile fTypeOut)
      assert $ inRes == outRes
    else assert $ isLeft result

prop_copy_twice :: FileType -> Property
prop_copy_twice fType = monadicIO $ do
  let options = def { inFileOpt = inFileNameTwice DBL64B fType
                    , inFileTypeOpt = Just fType
                    , outFileOpt = testFile fType
                    , outFileTypeOpt = Just fType
                    }
  result <- liftIO . runAppIO $ fileApp options
  assert $ isRight result
  let header = header3x3 { rows = 6 }
  inRes <- readDoubleList header (inFileNameTwice DBL64B fType)
  outRes <- readDoubleList header (testFile fType)
  liftIO $ removeFile (testFile fType)
  assert $ inRes == outRes


prop_convert_data_twice :: FileType -> DataType -> DataType -> Property
prop_convert_data_twice fType tIn tOut = monadicIO $ do
  let options = def { inFileOpt = inFileNameTwice tIn fType
                    , inDataTypeOpt = tIn
                    , inFileTypeOpt = Just fType
                    , outDataTypeOpt = Just tOut
                    , outFileOpt = testFile fType
                    , outFileTypeOpt = Just fType
                    }
  result <- liftIO . runAppIO $ fileApp options
  if legalConversion tIn tOut
    then do
      assert $ isRight result
      let inHeader = header3x3 { dataType = tIn }
      let outHeader = header3x3 { dataType = tOut }
      inRes <- readDoubleList inHeader (inFileNameTwice tIn fType)
      outRes <- readDoubleList outHeader (testFile fType)
      liftIO $ removeFile (testFile fType)
      assert $ inRes == outRes
    else do
      liftIO $ removeFile (testFile fType)
      assert $ isLeft result

fileTransformerTest :: TestTree
fileTransformerTest = $(testGroupGenerator)
