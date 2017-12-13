{-# LANGUAGE TemplateHaskell #-}

module School.App.Test.FileHandler
( fileHandlerTest ) where

import Conduit (liftIO)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (liftA2)
import Data.Default.Class (def)
import Data.Function (on)
import School.App.FileHandler
import School.FileIO.FileType (FileType(..))
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.TestUtils (testIOCatch)
import School.Types.TypeName (TypeName(..))
import System.Directory (removeFile)
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
                    , outHeader = header3x3
                    , outType = Just SM
                    }
  _ <- testIOCatch $ fileHandler options
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_copy_guess_filetypes :: Property
prop_copy_guess_filetypes = monadicIO $ do
  let options = def { inputFile = sm3x3File
                    , inHeader = header3x3
                    , outputFile = testFile
                    , outHeader = header3x3
                    }
  _ <- testIOCatch $ fileHandler options
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

prop_copy_add_extension :: Property
prop_copy_add_extension = monadicIO $ do
  let options = def { inputFile = sm3x3File
                    , inHeader = header3x3
                    , inType = Just SM
                    , outputFile = "test"
                    , outHeader = header3x3
                    , outType = Just SM
                    }
  _ <- testIOCatch $ fileHandler options
  equal <- liftIO $ fileEq sm3x3File testFile
  liftIO $ removeFile testFile
  assert equal

fileHandlerTest :: TestTree
fileHandlerTest = $(testGroupGenerator)
