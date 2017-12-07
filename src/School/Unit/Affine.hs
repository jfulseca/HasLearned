{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.Affine
( affine ) where

import Numeric.LinearAlgebra ((<>), R, add, tr')
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra (mapRows, sumRows)

affine :: Unit Double
affine = Unit
  { apply = affineApply
  , deriv = affineDeriv
  }

affineApply :: UnitParams R
            -> UnitActivation R
            -> UnitActivation R
affineApply AffineParams { affineBias, affineWeights }
            (BatchActivation inMatrix) =
  BatchActivation $ mapRows (add affineBias) (inMatrix <> tr' affineWeights)
affineApply _ (ApplyFail msg) = ApplyFail msg
affineApply _ _ = ApplyFail "Failure when applying affine unit"

failure :: String
        -> ( UnitGradient R
           , UnitParams R
           )
failure msg = (GradientFail msg, EmptyParams)

affineDeriv :: UnitParams R
            -> UnitGradient R
            -> UnitActivation R
            -> ( UnitGradient R
               , UnitParams R
               )
affineDeriv AffineParams { affineWeights }
            (BatchGradient inGrad)
            (BatchActivation input) =
  (outGrad, paramDerivs) where
    outGrad = BatchGradient $ inGrad <> affineWeights
    bias = sumRows inGrad
    weights = tr' inGrad <> input
    paramDerivs = AffineParams { affineBias = bias
                               , affineWeights = weights
                               }
affineDeriv _ (GradientFail msg) _ = failure msg
affineDeriv _ _ (ApplyFail msg) = failure msg
affineDeriv _ _ _ = failure "Failure when differentiating affine unit"
