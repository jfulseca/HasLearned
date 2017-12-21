{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.MultiNoulli
( multiNoulli ) where

import Conduit (ConduitM, mapC, mapMC)
import Control.Monad.Except (throwError)
import Numeric.LinearAlgebra (Container, Element, Vector, assoc, cols, fromRows,
                              rows, takeColumns, toColumns, toList, toLists)
import School.Train.AppTrain (AppTrain)
import School.Types.Error (Error)
import School.Types.Slinky (Slinky(..), slinkyAppend)
import School.Unit.CostParams (CostParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.CostFunction (CostFunction(..))

toTarget :: (Element a, RealFrac a)
         => Vector a
         -> CostParams
toTarget =
  BatchClassTarget . (map round) . toList

compute :: (Element a, Fractional a, Num a)
        => UnitActivation a
        -> Slinky CostParams
        -> Either Error a
compute (BatchActivation input)
        (SNode (BatchClassTarget target) _) =
  let factor = (-1) / (fromIntegral . rows $ input)
  in Right
   . (*factor)
   . sum
   . (zipWith (flip (!!)) target)
   . toLists
   $ input
compute _ _ = Left "MultiNoulli expects batch activation and batch class target"

deriv :: (Container Vector a, Fractional a, Num a)
      => UnitActivation a
      -> Slinky CostParams
      -> Either Error (UnitGradient a)
deriv (BatchActivation input)
      (SNode (BatchClassTarget target) _) =
  let factor = (-1) / (fromIntegral . rows $ input)
      c = cols input
  in Right
   . BatchGradient
   . fromRows
   . map (\idx -> assoc c 0 [(idx, factor)])
   $ target
deriv _ _ = Left "MultiNoulli expects batch activation and batch class target"

prepare :: (Element a, RealFrac a)
        => Maybe FilePath
        -> ConduitM (ForwardStack a)
                    (ForwardStack a)
                    (AppTrain a)
                    ()
prepare Nothing = mapMC $ \(activations, cParams) -> do
  case activations of
    [BatchActivation input] -> do
      let c = cols input
      let activation = takeColumns (c - 1) input
      let newParams = toTarget . last . toColumns $ input
      return ([BatchActivation activation], slinkyAppend newParams cParams)
    _ -> throwError "MultiNoulli setup expects single batch activation"
prepare _ = mapC id

multiNoulli :: (Container Vector a, RealFrac a)
            => Maybe FilePath
            -> CostFunction a
multiNoulli path = CostFunction { computeCost = compute
                                , derivCost = deriv
                                , prepareCost = prepare path
                                }
