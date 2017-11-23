module School.Unit.UnitForward
( ForwardStack
, unitForward
) where

import Conduit (ConduitM, mapMC)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain, getParams)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation)

type ForwardStack a = [UnitActivation a]

applyUnit :: Unit a 
          -> ForwardStack a
          -> AppTrain a (ForwardStack a)
applyUnit _ [] =
  throwError $ "No input activations " ++
               " to forward network unit "
applyUnit unit activations = do
  let input = head activations
  params <- getParams
  let output = apply unit params input
  return $ output:activations

unitForward :: Unit a
            -> ConduitM (ForwardStack a)
                        (ForwardStack a)
                        (AppTrain a)
                        ()
unitForward unit = mapMC (applyUnit unit)
