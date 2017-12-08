{-# LANGUAGE FlexibleContexts, NamedFieldPuns, UndecidableInstances #-}

module School.Unit.CostFunction
( CostFunction(..) ) where

import Conduit ((.|), ConduitM, mapC)
import Numeric.LinearAlgebra (Container, Vector, add, cols, rows)
import School.Train.AppTrain (AppTrain)
import School.Unit.CostParams (LinkedParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra (zeroMatrix)

data CostFunction a =
  CostFunction { computeCost :: UnitActivation a
                             -> LinkedParams
                             -> Either String a
               , derivCost :: UnitActivation a
                           -> LinkedParams
                           -> Either String (UnitGradient a)
               , setupCost :: ConduitM (UnitActivation a)
                                       (ForwardStack a)
                                       (AppTrain a)
                                       ()
               }

instance (Container Vector a, Num a) => Monoid (CostFunction a) where
  CostFunction { computeCost = c1, derivCost = d1, setupCost = s1 } `mappend`
    CostFunction { computeCost = c2, derivCost = d2, setupCost = s2 } =
    CostFunction { computeCost = c3, derivCost = d3, setupCost = s3 }
    where c3 activations p1@(Node _ p2) = do
            res1 <- c1 activations p1
            res2 <- c2 activations p2
            return $ res1 + res2
          c3 _ _ = Left "mappend costfunctions needs two cost params"
          d3 activations p1@(Node _ p2) = do
            (BatchGradient res1) <- d1 activations p1
            (BatchGradient res2) <- d2 activations p2
            return $ BatchGradient $
              add res1 res2
          d3 _ _ = Left "mappend costfunctions needs two cost params"
          s3 = s1 .| mapC head .| s2
  mempty = CostFunction { computeCost
                        , derivCost 
                        , setupCost
                        } where
    computeCost _ _ = Right 0
    derivCost (BatchActivation g) _ =
      Right . BatchGradient $ zeroMatrix r c
      where r = rows g
            c = cols g
    derivCost _ _ = Left "mempty deriv cost expects batch activation"
    setupCost = mapC pure
