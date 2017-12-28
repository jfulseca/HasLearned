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
import School.Utils.Monad (headMonad)

type BackwardStack a = ( [UnitActivation a]
                       , UnitGradient a
                       , a
                       )

derivUnit :: Unit a
          -> BackwardStack a
          -> AppTrain a (BackwardStack a)
derivUnit _ (_, GradientFail msg, _) = throwError msg
derivUnit unit (activations, inGrad, cost) = do
  input <- headMonad activations "No input activations to backward unit"
  case input of
    (ApplyFail msg) -> throwError msg
    _ -> do
      params <- getParams
      let (gradient, derivs) = deriv unit params inGrad input
      putParamDerivs derivs
      return (tail activations, gradient, cost)

unitBackward :: Unit a -> ConduitM (BackwardStack a)
                                   (BackwardStack a)
                                   (AppTrain a)
                                   ()

unitBackward unit = mapMC (derivUnit unit)
