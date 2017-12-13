{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.App.Test.FileHandler
( fileHandlerTest ) where

import Conduit ((.|), await, liftIO)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (liftA2)
import Data.Default.Class (def)
import Data.Function (on)
import Numeric.LinearAlgebra ((?))
import School.App.FileHandler
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource (matrixDoubleSource)
import School.TestUtils (assertRight, dummyMatrix, testIOCatch, testRun)
import School.Types.TypeName (TypeName(..))
import System.Posix.Redirect (redirectStderr)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

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
  let options = def { inputFile = sm3x3File
                    , inHeader = header3x3
                    , inType = Just SM
                    , outputFile = testFile
                    , outType = Just SM
                    }
  result <- testIOCatch $ fileHandler options
  assert $ result == ExitSuccess
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_copy_guess_filetypes :: Property
prop_copy_guess_filetypes = monadicIO $ do
  let options = def { inputFile = sm3x3File
                    , inHeader = header3x3
                    , outputFile = testFile
                    }
  result <- testIOCatch $ fileHandler options
  assert $ result == ExitSuccess
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_copy_add_extension :: Property
prop_copy_add_extension = monadicIO $ do
  let options = def { inputFile = sm3x3File
                    , inHeader = header3x3
                    , inType = Just SM
                    , outputFile = "test"
                    , outType = Just SM
                    }
  result <- testIOCatch $ fileHandler options
  assert $ result == ExitSuccess
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_skip_rows :: Property
prop_skip_rows = monadicIO $ do
  let outHeader = header3x3 { rows = 1 }
  let options = def { inputFile = sm3x3File
                    , inHeader = header3x3
                    , inType = Just SM
                    , outputFile = testFile
                    , outType = Just SM
                    , skipRows = Just 2
                    }
  result <- testIOCatch $ fileHandler options
  assert $ result == ExitSuccess
  let check = Just $ dummyMatrix 3 3 ? [2]
  readRes <- testRun $ matrixDoubleSource SM
                                          outHeader
                                          testFile
                   .| await
  liftIO $ removeFile testFile
  assertRight (== check) readRes

prop_skip_too_many_rows :: Property
prop_skip_too_many_rows = monadicIO $ do
  let options = def { inHeader = header3x3
                    , inType = Just SM
                    , skipRows = Just 4
                    }
  let handler = redirectStderr $ fileHandler options
  result <- testIOCatch $ handler
  assert $ result == ExitFailure 1

fileHandlerTest :: TestTree
fileHandlerTest = $(testGroupGenerator)
