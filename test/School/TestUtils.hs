{-# LANGUAGE NamedFieldPuns #-}

module School.TestUtils
( dummyHeader
, dummyList
, dummyMatrix
, getRandDouble
, randomAffineParams
, randomMatrix
, randomVector
, testRun
, whenPrint
) where

import Conduit (ConduitM)
import Control.Monad (when)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO, runAppIO)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.DoubleConversion (doubleRange)
import School.Types.TypeName (TypeName(INT))
import School.Unit.UnitParams (UnitParams(..))
import System.Random (getStdRandom, randomR)
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

getRandDouble :: IO Double
getRandDouble =
  getStdRandom (randomR doubleRange)

matMax :: Double
matMax = 1e3

randomMatrix :: Int -> Int -> IO (Matrix R)
randomMatrix nRows nCols = do
  let nEls = nRows * nCols
  inputList <- sequence . (replicate nEls) $ getStdRandom (randomR (-matMax, matMax))
  return $ (nRows >< nCols) inputList

randomVector :: Int -> IO (Vector R)
randomVector n = do
  inputList <- sequence . (replicate n) $ getStdRandom (randomR (-matMax, matMax))
  return $ n |> inputList

randomAffineParams :: Int
                   -> Int
                   -> IO (UnitParams R)
randomAffineParams fSize oSize = do
  affineBias <- randomVector oSize
  affineWeights <- randomMatrix oSize fSize
  return AffineParams { affineBias
                      , affineWeights
                      }

toPrint :: (Show a) => String -> a -> IO ()
toPrint tag val = putStrLn $ tag ++ " " ++ (show val)

whenPrint :: (Show a) => Bool -> [String] -> [a] -> IO ()
whenPrint cond tags vals = do
  when cond (sequence_ $ zipWith toPrint tags vals)
