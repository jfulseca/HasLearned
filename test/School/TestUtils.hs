{-# LANGUAGE NamedFieldPuns #-}

module School.TestUtils
( CostFunc
, diffInput
, dummyHeader
, dummyList
, dummyMatrix
, getRandDouble
, jTest
, matIndexes
, randomAffineParams
, randomMatrix
, randomVector
, testRun
, whenPrint
) where

import Conduit (ConduitM)
import Control.Monad (when)
import Data.Void (Void)
import Numeric.LinearAlgebra ((><), (|>), IndexOf, Matrix, R, Vector, accum, sumElements)
import School.FileIO.AppIO (AppIO, runAppIO)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.DoubleConversion (doubleRange)
import School.Types.TypeName (TypeName(INT))
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import System.Random (getStdRandom, randomR)
import Test.QuickCheck.Modifiers (Positive(..))
import Test.QuickCheck.Monadic (PropertyM, run)

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

type CostFunc = UnitActivation R -> Double

jTest :: CostFunc
jTest (BatchActivation m) = sumElements m
jTest _ = 0

diffInput :: Unit R
          -> UnitParams R
          -> UnitActivation R
          -> Double
          -> IndexOf Matrix
          -> Double
diffInput unit params input eps idx =
  (jAdd - jSub) / (2*eps) where
    outAdd = apply unit params (alterInput eps idx input)
    outSub = apply unit params (alterInput (-eps) idx input)
    jAdd = jTest outAdd
    jSub = jTest outSub

alterInput :: AlterInput
alterInput change idx (BatchActivation m) =
  BatchActivation $ accum m (+) [(idx, change)]
alterInput _ _ _ = ApplyFail "alterInput error"

type AlterInput = Double
               -> IndexOf Matrix
               -> UnitActivation R
               -> UnitActivation R

matIndexes :: Int -> Int -> [IndexOf Matrix]
matIndexes r c = [ (j, k) | j <- [0..r-1], k <- [0..c-1] ] 

