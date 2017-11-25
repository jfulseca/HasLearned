module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, mapMC)
import Control.Monad.Except (throwError)
import School.Train.AppTrain (AppTrain, putCost)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)

costApplication :: CostFunction a
                -> ForwardStack a
                -> AppTrain a (BackwardStack a)
costApplication costFunc stack = do
  let cost = computeCost costFunc (head stack) 
  either throwError putCost cost
  let grad = derivCost costFunc (head stack)
  either throwError
         (\g -> return (tail stack, g))
         grad

applyCost :: CostFunction a
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppTrain a)
                      ()
applyCost cost = mapMC (costApplication cost)
