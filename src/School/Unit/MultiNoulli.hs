{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.MultiNoulli
( multiNoulli ) where

import Conduit (mapMC)
import Numeric.LinearAlgebra (Container, Element, Vector, assoc, cols, fromRows,
                              rows, takeColumns, toColumns, toList, toLists)
import School.Train.AppTrain (putCostParams)
import School.Types.Slinky (Slinky(..))
import School.Unit.CostParams (CostParams(..))
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
                  (SNode (BatchClassTarget target) _) =
          Right
        . (*(-1))
        . ((flip (/)) (fromIntegral . rows $ input))
        . sum
        . (zipWith (flip (!!)) target)
        . toLists
        $ input
      computeCost _ _ = Left "MultiNoulli expects batch activation and batch classification target"
      derivCost (BatchActivation input)
                (SNode (BatchClassTarget target) _) =
        let factor = (-1) / (fromIntegral . rows $ input)
            c = cols input in
          Right
        . BatchGradient
        . fromRows
        . map (\idx -> assoc c 0 [(idx, factor)])
        $ target
      derivCost _ _ = Left "MultiNoulli expects batch activation and batch class target"
      setupCost = mapMC $ \(BatchActivation input) -> do
        let c = cols input
        let activations = takeColumns (c - 1) input
        let newParams = toTarget . last . toColumns $ input
        putCostParams newParams
        return [BatchActivation activations]
  in CostFunction { computeCost
                  , derivCost
                  , setupCost
                  }
