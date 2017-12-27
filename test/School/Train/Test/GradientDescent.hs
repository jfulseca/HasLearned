{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Train.Test.GradientDescent
( gradientDescentTest ) where

import Conduit ((.|), liftIO, runConduit, sinkNull, yield, yieldMany)
import Data.Either (isLeft)
import Numeric.LinearAlgebra ((><))
import School.TestUtils (addClasses, assertRight, empty, isSorted,
                         randomAffineParams, randomNNInts, randomMatrix,
                         unitCorrect, weight1)
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileType (FileType, toExtension)
import School.FileIO.FileHeader (FileHeader(..))
import School.FileIO.MatrixSink (matrixIntSink)
import School.Train.AppTrain (AppTrain)
import School.Train.GradientDescent
import School.Train.SimpleDescentUpdate (simpleDescentUpdate)
import School.Train.StoppingCondition (maxIterations)
import School.Train.IterationHandler (storeCost)
import School.Train.TrainState (TrainState(..), HandlerStore(..), def)
import School.Types.PingPong (pingPongSingleton)
import School.Types.DataType (DataType(..))
import School.Types.Error (Error)
import School.Types.FloatEq ((~=))
import School.Unit.CostFunction (CostFunction)
import School.Unit.Affine (affine)
import School.Unit.MultiNoulli (multiNoulli)
import School.Unit.RecLin (recLin)
import School.Unit.UnitParams (UnitParams(..))
import System.Directory (removeFile)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

prop_no_units :: Property
prop_no_units = monadicIO $ do
  result <- liftIO $ gradientDescent (yield empty)
                                     []
                                     weight1
                                     simpleDescentUpdate
                                     mempty
                                     mempty
                                     def
  assert $ isLeft result

prop_iterations :: Positive Int -> Positive Int -> Positive Int -> Property
prop_iterations (Positive n) (Positive b) (Positive f) = monadicIO $ do
  input <- liftIO $ randomMatrix b f
  let source =  yieldMany . repeat $ input
  result <- liftIO $ gradientDescent source
                                    [recLin]
                                    weight1
                                    simpleDescentUpdate
                                    (maxIterations n)
                                    mempty
                                    def
  assertRight ((== n) . iterationCount) result

prop_cost_decline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_cost_decline (Positive b) (Positive f) (Positive o) = monadicIO $ do
  input <- liftIO $ randomMatrix b f
  let source =  yieldMany . repeat $ input
  paramList <- liftIO $ pingPongSingleton <$> randomAffineParams f o
  let initState = def { handlerStore = CostList []
                                  , learningRate = 1e-2
                                  , paramList
                                  }
  result <- liftIO $ gradientDescent source
                                     [affine]
                                     weight1
                                     simpleDescentUpdate
                                     (maxIterations 5)
                                     storeCost
                                     initState
  assertRight ((\(CostList c) -> isSorted c) . handlerStore)
              result

multiSingle :: CostFunction Double (AppTrain a)
multiSingle = multiNoulli Nothing Nothing

prop_multinoulli_single_file :: Positive Int -> Positive Int -> Property
prop_multinoulli_single_file (Positive c) (Positive b) = monadicIO $ do
  activation <- liftIO $ randomMatrix b c
  classes <- liftIO $ randomNNInts (c - 1) b
  let input = addClasses classes activation
  let source =  yieldMany . repeat $ input
  let initState = def { handlerStore = CostList []
                      , paramList = pingPongSingleton EmptyParams
                      }
  result <- liftIO $ gradientDescent source
                                     [unitCorrect classes]
                                     multiSingle
                                     simpleDescentUpdate
                                     (maxIterations 5)
                                     storeCost
                                     initState
  let check = CostList $ replicate 5 (-1)
  assertRight ((~= check) . handlerStore) result

multiTwo :: [Int]
         -> Int
         -> FileType
         -> IO ( Either Error ( FilePath
                              , CostFunction Double (AppTrain a)))
multiTwo classes iterations fType = do
  let fName = "test." ++ (toExtension fType)
  let cols = 1
  let nClasses = length classes
  let rows = iterations * nClasses
  let header = FileHeader { dataType = INT32B, cols, rows }
  let matrix = (rows >< cols) . concat . (replicate iterations)
             . (map fromIntegral) $ classes
  writeRes <- runAppIO . runConduit $ yield matrix
                                   .| matrixIntSink fType header fName
                                   .| sinkNull
  let multi = multiNoulli (Just fName) (Just $ header { rows = nClasses })
  either (return . Left)
         (return . pure . Right $ (fName, multi))
         writeRes

prop_multinoulli_two_files :: Positive Int -> Positive Int -> FileType -> Property
prop_multinoulli_two_files (Positive c) (Positive b) fType = monadicIO $ do
  activation <- liftIO $ randomMatrix b c
  classes <- liftIO $ randomNNInts (c - 1) b
  let input = addClasses classes activation
  let source =  yieldMany . repeat $ input
  let initState = def { handlerStore = CostList []
                      , paramList = pingPongSingleton EmptyParams
                      }
  let iterations = 5
  multiResult <- liftIO $ multiTwo classes iterations fType
  either (\e -> do
            liftIO . putStrLn $ "ERROR " ++ e
            assert False)
         (\(fName, multi) -> do
            result <- liftIO $ gradientDescent source
                                               [unitCorrect classes]
                                               multi
                                               simpleDescentUpdate
                                               (maxIterations iterations)
                                               storeCost
                                               initState
            let check = CostList $ replicate iterations (-1)
            liftIO $ removeFile fName
            assertRight ((~= check) . handlerStore) result
          )
          multiResult

gradientDescentTest :: TestTree
gradientDescentTest = $(testGroupGenerator)
