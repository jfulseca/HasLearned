{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.CostFunction
( SetupCost
, CostFunction(..)
, defSetupCost
) where

import Conduit ((.|), ConduitM, ZipSource(..), getZipSource, mapC, mapMC)
import Numeric.LinearAlgebra (Container, Matrix, Vector, add, cols, rows)
import School.Types.Slinky (Slinky(..), slinkyConcat)
import School.Unit.CostParams (CostParams)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra (zeroMatrix)

type SetupCost a m = ConduitM () (Matrix a) m ()
                  -> ConduitM () (ForwardStack a) m ()

defSetupCost :: (Monad m) => SetupCost a m
defSetupCost source =
  source .| mapMC (\matrix -> return ([BatchActivation matrix], SNil))

type CompupteCost a = UnitActivation a
                   -> Slinky CostParams
                   -> Either String a

type DerivCost a = UnitActivation a
                -> Slinky CostParams
                -> Either String (UnitGradient a)

type PrepareCost m a =
  ConduitM (ForwardStack a) (ForwardStack a) m ()

data CostFunction a m =
  CostFunction { computeCost :: CompupteCost a
               , derivCost :: DerivCost a
               , prepareCost :: PrepareCost m a
               , setupCost :: SetupCost a m
               }

mappendCompute :: (Num a)
               => CompupteCost a
               -> CompupteCost a
               -> CompupteCost a
mappendCompute c1 c2 activations params1@(SNode _ params2) = do
  res1 <- c1 activations params1
  res2 <- c2 activations params2
  return $ res1 + res2
mappendCompute _ _ _ _ = Left "mappend costfunctions needs two cost params"

mappendDeriv :: (Container Vector a, Num a)
             => DerivCost a
             -> DerivCost a
             -> DerivCost a
mappendDeriv d1 d2 activations params1@(SNode _ params2) = do
  (BatchGradient res1) <- d1 activations params1
  (BatchGradient res2) <- d2 activations params2
  return $ BatchGradient $ add res1 res2
mappendDeriv _ _ _ _ = Left "mappend costfunctions needs two cost params"

combinator :: ForwardStack a -> ForwardStack a -> ForwardStack a
combinator (_, cParams1) (activations, cParams2) =
  (activations, slinkyConcat cParams1 cParams2)

mappendSetup :: (Monad m)
             => SetupCost a m
             -> SetupCost a m
             -> SetupCost a m
mappendSetup s1 s2 source = getZipSource $ combinator
                                       <$> (ZipSource $ s1 source)
                                       <*> (ZipSource $ s2 source)

instance (Container Vector a, Num a, Monad m) => Monoid (CostFunction a m) where
  mappend CostFunction { computeCost = c1
                       , derivCost = d1
                       , prepareCost = p1
                       , setupCost = s1
                       }
          CostFunction { computeCost = c2
                       , derivCost = d2
                       , prepareCost = p2
                       , setupCost = s2
                       } =
   CostFunction { computeCost = mappendCompute c1 c2
                , derivCost = mappendDeriv d1 d2
                , prepareCost = p1 .| p2
                , setupCost = mappendSetup s1 s2
                }
  mempty = CostFunction { computeCost
                        , derivCost
                        , prepareCost = mapC id
                        , setupCost = defSetupCost
                        } where
    computeCost _ _ = Right 0
    derivCost (BatchActivation g) _ =
      Right . BatchGradient $ zeroMatrix r c
      where r = rows g
            c = cols g
    derivCost _ _ = Left "mempty deriv cost expects batch activation"
