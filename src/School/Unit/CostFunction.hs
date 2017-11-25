{-# LANGUAGE FlexibleContexts, NamedFieldPuns, UndecidableInstances #-}

module School.Unit.CostFunction
( CostFunction(..) ) where

import Numeric.LinearAlgebra (Container, Vector, add, cols, rows)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra (zeroMatrix)

data CostFunction a =
  CostFunction { computeCost :: UnitActivation a
                             -> Either String a
               , derivCost :: UnitActivation a
                           -> Either String (UnitGradient a)
               }

instance (Container Vector a, Num a) => Monoid (CostFunction a) where
  mappend CostFunction { computeCost = c1, derivCost = d1 }
          CostFunction { computeCost = c2, derivCost = d2 } =
    CostFunction { computeCost, derivCost } where
      computeCost activation = do
        j1 <- c1 activation
        j2 <- c2 activation
        return $ j1 + j2
      derivCost activation = do
        (BatchGradient g1) <- d1 activation
        (BatchGradient g2) <- d2 activation
        return . BatchGradient $ g1 `add` g2
  mempty = CostFunction { computeCost, derivCost } where
    computeCost = const (Right 0)
    derivCost (ApplyFail msg) = Left msg
    derivCost (BatchActivation activation) =
      let r = rows activation
          c = cols activation in
      Right . BatchGradient $ zeroMatrix r c
