module School.Unit.RecLin
( recLin ) where

import Numeric.LinearAlgebra (R, cmap, cond)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))

recLin :: Unit R
recLin = Unit { apply = recLinApply
              , deriv = recLinDeriv
              }

recLinApply :: UnitParams R
            -> UnitActivation R
            -> UnitActivation R
recLinApply EmptyParams (BatchActivation inMatrix) =
  BatchActivation $ cmap (max 0) inMatrix
recLinApply _ f@(ApplyFail _) = f
recLinApply _ _ = ApplyFail "Failure when applying RecLin unit"

recLinDeriv :: UnitParams R
            -> UnitGradient R
            -> UnitActivation R
            -> ( UnitGradient R
               , UnitParams R)
recLinDeriv EmptyParams
            (BatchGradient inGrad)
            (BatchActivation input)
  = (BatchGradient grad, EmptyParams)
    where grad = cond input 0 0 0 inGrad
recLinDeriv EmptyParams f@(GradientFail _) _ = (f, EmptyParams)
recLinDeriv EmptyParams _ (ApplyFail msg) = (GradientFail msg, EmptyParams)
recLinDeriv _ _ _ = (GradientFail "Failure when differentiating RecLin unit" , EmptyParams)
