{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.MultiNoulli
( multiNoulli ) where

import Conduit (mapMC)
import Control.Monad.Except (throwError)
import Numeric.LinearAlgebra (Container, Element, Vector, assoc, cols, fromRows,
                              rows, takeColumns, toColumns, toList, toLists)
import School.Types.Slinky (Slinky(..), slinkyAppend)
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
      setupCost = mapMC $ \(activations, cParams) -> do
        case activations of
          [BatchActivation input] -> do
            let c = cols input
            let activation = takeColumns (c - 1) input
            let newParams = toTarget . last . toColumns $ input
            return ([BatchActivation activation], slinkyAppend newParams cParams)
          _ -> throwError "MultiNoulli setup expects single batch activation"
  in CostFunction { computeCost
                  , derivCost
                  , setupCost
                  }
