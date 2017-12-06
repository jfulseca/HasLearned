{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.MultiNoulli
( multiNoulli ) where

import Conduit (mapMC)
import Numeric.LinearAlgebra (Container, Element, Vector, assoc, cols, fromRows,
                              rows, takeColumns, toColumns, toList, toLists)
import School.Train.AppTrain (putCostParams)
import School.Train.TrainState (CostParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.CostFunction (CostFunction(..))

toTarget :: (Element a, RealFrac a)
         => Vector a
         -> CostParams
toTarget =
  BatchClassTarget . (map round) . toList

multiNoulli :: (Container Vector a, RealFrac a)
            => CostFunction a
multiNoulli =
  let computeCost (BatchActivation input)
                  (BatchClassTarget target) =
          Right
        . (*(-1))
        . ((flip (/)) (fromIntegral . rows $ input))
        . sum
        . (zipWith (flip (!!)) target)
        . toLists
        $ input
      computeCost _ _ = Left "Weight decay expects batch activation and batch classification target"
      derivCost (BatchActivation input)
                (BatchClassTarget target) =
        let factor = (-1) / (fromIntegral . rows $ input)
            c = cols input in
          Right
        . BatchGradient
        . fromRows
        . map (\idx -> assoc c 0 [(idx, factor)])
        $ target
      derivCost _ _ = Left "Weight decay expects batch activation and no cost params"
      setupCost = mapMC $ \(BatchActivation input) -> do
        let c = cols input
        let activations = takeColumns (c - 1) input
        putCostParams . toTarget . last . toColumns $ input
        return [BatchActivation activations]
  in CostFunction { computeCost
                  , derivCost
                  , setupCost
                  }
