{-# LANGUAGE FlexibleContexts, NamedFieldPuns, UndecidableInstances #-}

module School.Unit.CostFunction
( AlterConduit
, CostFunction(..)
) where

import Conduit ((.|), ConduitM, mapC)
import Data.ByteString (ByteString)
import Numeric.LinearAlgebra (Container, Vector, add, cols, rows)
import School.Types.Slinky (Slinky(..))
import School.Unit.CostParams (CostParams)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra (zeroMatrix)


type AlterConduit a m =
     ConduitM (ByteString) (ForwardStack a) m ()
  -> ConduitM (ByteString) (ForwardStack a) m ()

data CostFunction a m =
  CostFunction { computeCost :: UnitActivation a
                             -> Slinky CostParams
                             -> Either String a
               , derivCost :: UnitActivation a
                           -> Slinky CostParams
                           -> Either String (UnitGradient a)
               , prepareCost :: ConduitM (ForwardStack a)
                                         (ForwardStack a)
                                         m
                                         ()
               , alterConduit :: AlterConduit a m
               }

instance (Container Vector a, Num a, Monad m) => Monoid (CostFunction a m) where
  mappend CostFunction { computeCost = c1
                       , derivCost = d1
                       , prepareCost = p1
                       , alterConduit = a1
                       }
          CostFunction { computeCost = c2
                       , derivCost = d2
                       , prepareCost = p2
                       , alterConduit = a2
                       } =
   CostFunction { computeCost = c3
                , derivCost = d3
                , prepareCost = p3
                , alterConduit = undefined
                }
    where c3 activations params1@(SNode _ params2) = do
            res1 <- c1 activations params1
            res2 <- c2 activations params2
            return $ res1 + res2
          c3 _ _ = Left "mappend costfunctions needs two cost params"
          d3 activations params1@(SNode _ params2) = do
            (BatchGradient res1) <- d1 activations params1
            (BatchGradient res2) <- d2 activations params2
            return $ BatchGradient $
              add res1 res2
          d3 _ _ = Left "mappend costfunctions needs two cost params"
          p3 = p1 .| p2
  mempty = CostFunction { computeCost
                        , derivCost
                        , prepareCost
                        , alterConduit = id
                        } where
    computeCost _ _ = Right 0
    derivCost (BatchActivation g) _ =
      Right . BatchGradient $ zeroMatrix r c
      where r = rows g
            c = cols g
    derivCost _ _ = Left "mempty deriv cost expects batch activation"
    prepareCost = mapC id
