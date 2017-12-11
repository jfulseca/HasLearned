{-# LANGUAGE NamedFieldPuns #-}

module School.App.MultiLayerPerceptron
( MLPOptions(..)
, multiLayerPerceptron
) where

import Conduit ((.|), mapC)
import Numeric.LinearAlgebra (R)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.MatrixSource (matrixDoubleSource)
import School.Train.GradientDescent (gradientDescent)
import School.Train.IterationHandler (logCost)
import School.Train.SimpleDescentUpdate (simpleDescentUpdate)
import School.Train.StoppingCondition (maxIterations)
import School.Train.TrainState (TrainState(..), defTrainState)
import School.Types.FloatEq ((~=))
import School.Types.PingPong (PingPong, toPingPong)
import School.Types.PosInt (PosInt, getPosInt)
import School.Types.TypeName (TypeName(..))
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.MultiNoulli (multiNoulli)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Unit.WeightDecay (weightDecay)
import School.Utils.Random (randAffineParams)
import System.FilePath (FilePath)
import System.Random (StdGen, mkStdGen)

data MLPOptions =
  MLPOptions { hiddenDims :: [Int]
             , initRate :: R
             , maxIter :: Int
             , nBatches :: PosInt
             , nClasses :: PosInt
             , nFeatures :: PosInt
             , randomSeed :: Int
             , weightDecayCoeff :: R
             , writeCost :: Maybe FilePath
             }

heLimits :: Int -> (R, R)
heLimits d = (0, sqrt (2 / (fromIntegral d)))

getUnits :: [Int]
         -> StdGen
         -> ([Unit R], [UnitParams R])
         -> ([Unit R], [UnitParams R])
getUnits (dim1:dim2:dim3:dims) gen (units, params) =
  getUnits (dim2:dim3:dims) gen' acc where
    addUnits = [affine, recLin]
    limits = heLimits dim1
    (affineParams, gen') =
      randAffineParams dim1 dim2 limits gen
    addParams = [affineParams, EmptyParams]
    acc = (units ++ addUnits, params ++ addParams)
getUnits (dim1:dim2:_) gen (units, params) =
  (units ++ [addUnit], params ++ [addParam]) where
    addUnit = affine
    limits = heLimits dim1
    (addParam, _) =
      randAffineParams dim1 dim2 limits gen
getUnits [_] _ acc = acc
getUnits [] _ acc = acc

setupUnits :: Int
           -> Int
           -> [Int]
           -> Int
           -> Either String
                     ([Unit R], PingPong (UnitParams R))
setupUnits nFeatures nClasses hiddenDims seed = do
  let allDims = nFeatures:hiddenDims ++ [nClasses]
  let gen = mkStdGen seed
  let (units, paramList) = getUnits allDims
                                    gen
                                    ([], [])
  params <- toPingPong paramList
  return (units, params)

multiLayerPerceptron :: MLPOptions
                     -> FilePath
                     -> IO (Either String
                                   (Maybe [R], [UnitParams R]))
multiLayerPerceptron (MLPOptions { nClasses = nC
                                 , nBatches = nB
                                 , nFeatures = nF
                                 , hiddenDims
                                 , initRate = learningRate
                                 , maxIter
                                 , randomSeed
                                 , weightDecayCoeff
                                 , writeCost
                                 })
                      path = do
  let header = MatrixHeader { cols = nF
                            , rows = nB
                            , dataType = DBL64B
                            }
  let source = matrixDoubleSource header path
            .| mapC BatchActivation
  let setup = setupUnits (getPosInt nF)
                         (getPosInt nC)
                         hiddenDims
                         randomSeed
  case setup of
    Left e -> return . Left $ e
    Right (units, paramList) -> do
      let cost = if (weightDecayCoeff ~= 0)
                   then multiNoulli
                   else multiNoulli `mappend` (weightDecay weightDecayCoeff)
      let update = simpleDescentUpdate
      let condition = maxIterations maxIter
      let handler = case writeCost of
                      Nothing -> mempty
                      Just p -> logCost p
      let initState = defTrainState { paramList
                                    , learningRate
                                    }
      endState <- gradientDescent source
                                  units
                                  cost
                                  update
                                  condition
                                  handler
                                  initState
      return $ Left ""
