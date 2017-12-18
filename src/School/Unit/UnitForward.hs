module School.Unit.UnitForward
( ForwardStack
, unitForward
) where

import Conduit (ConduitM, mapMC)
import Control.Monad.Except (throwError)
import School.App.AppS (AppS)
import School.Train.AppTrain (getParams)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))

type ForwardStack a = [UnitActivation a]

applyUnit :: Unit a 
          -> ForwardStack a
          -> AppS a (ForwardStack a)
applyUnit _ [] =
  throwError $ "No input activations " ++
               " to forward network unit "
applyUnit unit activations = do
  let input = head activations
  case input of
    (ApplyFail msg) -> throwError msg
    _ -> do
      params <- getParams
      let output = apply unit params input
      return $ output:activations

unitForward :: Unit a -> ConduitM (ForwardStack a)
                                  (ForwardStack a)
                                  (AppS a)
                                  ()
unitForward unit = mapMC (applyUnit unit)
