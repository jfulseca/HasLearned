{-# LANGUAGE FlexibleContexts, NamedFieldPuns, UndecidableInstances #-}

module School.Unit.CostFunction
( CostFunction(..) ) where

import Conduit (ConduitM)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (CostParams)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitGradient (UnitGradient(..))

data CostFunction a =
  CostFunction { computeCost :: UnitActivation a
                             -> CostParams a
                             -> Either String a
               , derivCost :: UnitActivation a
                           -> CostParams a
                           -> Either String (UnitGradient a)
               , setupCost :: ConduitM (UnitActivation a)
                                       (ForwardStack a)
                                       (AppTrain a)
                                       ()
               }

