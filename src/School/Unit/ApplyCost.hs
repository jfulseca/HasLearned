module School.Unit.ApplyCost
( applyCost ) where

import Conduit (ConduitM, mapMC)
import School.Train.AppTrain (AppTrain, putCost)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.UnitBackward (BackwardStack)
import School.Unit.UnitForward (ForwardStack)

costApplication :: CostFunction a
                -> ForwardStack a
                -> AppTrain a (BackwardStack a)
costApplication cost stack = do
  putCost $ computeCost cost (head stack) 
  let grad = derivCost cost (head stack)
  return ( tail stack
         , grad
         )

applyCost :: CostFunction a
          -> ConduitM (ForwardStack a)
                      (BackwardStack a)
                      (AppTrain a)
                      ()
applyCost cost = mapMC (costApplication cost)
