{-# LANGUAGE BangPatterns, FlexibleContexts, NamedFieldPuns #-}

module School.Unit.LogSoftMax
( logSoftMax ) where

import Numeric.LinearAlgebra (Container, Matrix, R, Vector,
                              add, atIndex, build, cmap, size, sumElements)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra (mapRows, sumCols)

logSoftMax :: Unit R
logSoftMax = Unit
  { apply = lsmApply
  , deriv = lsmDeriv
  }

getNorm :: (Container Vector a, Floating a)
        => Vector a
        -> a
getNorm = log . sumElements . (cmap exp)

handleRow :: (Container Vector a, Floating a)
          => Vector a
          -> Vector a
handleRow v = let !norm = getNorm v
  in cmap (flip (-) $ norm) v

lsmApply :: UnitParams R
         -> UnitActivation R
         -> UnitActivation R
lsmApply EmptyParams (BatchActivation inMatrix) =
  BatchActivation $ mapRows handleRow inMatrix
lsmApply _ (ApplyFail msg) = ApplyFail msg
lsmApply _ _ = ApplyFail "Failure when applying affine unit"

failure :: String
        -> ( UnitGradient R
           , UnitParams R
           )
failure msg = (GradientFail msg, EmptyParams)

builder :: (Container Vector a, RealFrac a)
        => Matrix a
        -> Vector a
        -> Vector a
        -> (a -> a -> a)
builder exps norms gradCols dj dk =
  let j = round dj :: Int
      k = round dk :: Int
  in (-1) * (atIndex exps (j, k))
   * (atIndex gradCols j)
   / (atIndex norms j)

lsmDeriv :: UnitParams R
         -> UnitGradient R
         -> UnitActivation R
         -> ( UnitGradient R
            , UnitParams R
            )
lsmDeriv EmptyParams
         (BatchGradient inGrad)
         (BatchActivation input) =
  (outGrad, EmptyParams) where
    !exps = cmap exp input
    !norms = sumCols exps
    !gradCols = sumCols inGrad
    delta = build (size input) (builder exps norms gradCols)
    outGrad = BatchGradient $ add inGrad delta
lsmDeriv _ (GradientFail msg) _ = failure msg
lsmDeriv _ _ (ApplyFail msg) = failure msg
lsmDeriv _ _ _ = failure "Failure when differentiating affine unit"
