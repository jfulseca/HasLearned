{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.Affine
( affine ) where

import Numeric.LinearAlgebra ((<>), R, tr')
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Utils.LinearAlgebra ((+>), sumCols)

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
  BatchActivation $ (inMatrix <> tr' affineWeights) +> affineBias
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
            (BatchGradient inJac)
            (BatchActivation input) =
  (outJac, paramDerivs) where
    outJac = BatchGradient $ inJac <> affineWeights
    bias = sumCols inJac
    weights = tr' inJac <> input
    paramDerivs = AffineParams { affineBias = bias
                               , affineWeights = weights
                               }
affineDeriv _ (GradientFail msg) _ = failure msg
affineDeriv _ _ (ApplyFail msg) = failure msg
affineDeriv _ _ _ = failure "Failure when differentiating affine unit"
