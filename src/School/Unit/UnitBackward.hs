module School.Unit.UnitBackward
( BackwardStack
, unitBackward ) where

import Conduit (ConduitM, mapMC)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain)
import School.Train.StateFunctions (getParams, putParamDerivs)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))

type BackwardStack a =
  ( [UnitActivation a]
  , UnitGradient a
  )

derivUnit :: Unit a
          -> BackwardStack a
          -> AppTrain a (BackwardStack a)
derivUnit _ ([], _) =
  throwError $ "No input activations " ++
          "to backward network unit "
derivUnit _ (_, GradientFail msg) = throwError msg
derivUnit unit (acts, inGrad) = do
  let input = head acts
  case input of
    (ApplyFail msg) -> throwError msg
    _ -> do
      params <- getParams
      let (gradient, derivs) = deriv unit params inGrad input
      putParamDerivs derivs
      return $ ( tail acts
               , gradient
               )

unitBackward :: Unit a -> ConduitM (BackwardStack a)
                                   (BackwardStack a)
                                   (AppTrain a)
                                   ()

unitBackward unit = mapMC (derivUnit unit)
