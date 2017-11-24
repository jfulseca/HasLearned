module School.Unit.UnitBackward
( unitBackward ) where

import Conduit (ConduitM, mapMC)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain, getParams, putParamDerivs)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))

type BackwardStack a =
  ( ForwardStack a
  , UnitGradient a
  )

derivUnit :: Unit a 
          -> BackwardStack a
          -> AppTrain a (BackwardStack a)
derivUnit _ (_, []) =
  throwError $ "ERROR: No input activations " ++
               " to backward network unit "
derivUnit unit (inGrad, acts) = do
  let input = head acts
  case input of
    (ApplyFail msg) -> throwError $ "ERROR: " ++ msg
    case inGrad of
      (GradientFail msg) -> throwError $ "ERROR: " ++ msg
      _ -> do
        params <- getParams
        let (gradient, derivs) =
          deriv unit params inGrad input
        putParamDerivs derivs
        return $ ( gradient
                 , tail acts
                 )

unitBackward :: Unit a -> ConduitM (BackwardStack a)
                                   (BackwardStack a)
                                   (AppTrain a)
                                   ()

unitBackward unit = mapMC (derivUnit unit)
