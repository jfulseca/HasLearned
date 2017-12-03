module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, await, yield)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import School.Train.AppTrain (AppTrain, putCost)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)

applyCost :: CostFunction a
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppTrain a)
                      ()
applyCost costFunc = do
  stack <- await
  let activations = fromMaybe [] stack
  when (length activations < 1) $ throwError "No activations in applyCost"
  let activation = head activations
  let cost = computeCost costFunc activation
  either throwError (lift . putCost) cost
  let grad = derivCost costFunc activation
  either throwError
    (\g -> yield (tail activations, g))
    grad
