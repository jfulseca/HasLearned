module School.TestUtils
( dummyHeader
, dummyList
, dummyMatrix
, randomMatrix
, randomVector
, testRun
) where

import Conduit (ConduitM)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO, runAppIO)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.TypeName (TypeName(INT))
import System.Random (getStdRandom, random)
import Test.QuickCheck.Modifiers (Positive(..))
import Test.QuickCheck.Monadic (PropertyM, run)
import Numeric.LinearAlgebra ((><), (|>), Matrix, R, Vector)

testRun :: ConduitM () Void AppIO a
        -> PropertyM IO (Either String a)
testRun = run . runAppIO

dummyHeader :: MatrixHeader
dummyHeader = MatrixHeader INT n n where
  n = Positive 1

dummyList :: (Num a) => Int -> Int -> [a]
dummyList r c = map fromIntegral [ r*i + j | i <- [1..r], j <- [1..c] ]

dummyMatrix :: Int -> Int -> Matrix Double
dummyMatrix r c = r >< c $ dummyList r c

randomMatrix :: Int -> Int -> IO (Matrix R)
randomMatrix nRows nCols = do
  let nEls = nRows * nCols
  inputList <- sequence . (replicate nEls) $ getStdRandom random :: IO [Double]
  return $ (nRows >< nCols) inputList

randomVector :: Int -> IO (Vector R)
randomVector n = do
  inputList <- sequence . (replicate n) $ getStdRandom random :: IO [Double]
  return $ n |> inputList
