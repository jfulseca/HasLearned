{-# LANGUAGE NamedFieldPuns #-}

module School.Evaluate.Evaluate
( evaluate ) where

import Conduit ((.|), sinkList)
import School.App.AppS (runAppSConduit)
import School.Train.ForwardPass (forwardPass)
import School.Train.TrainState (TrainState(..), def)
import School.Types.PingPong (PingPong)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (ActivationSource)
import School.Unit.UnitParams (UnitParams)
import School.Utils.Either (mapRight)

evaluate :: (Num a)
         => [Unit a]
         -> CostFunction a
         -> PingPong (UnitParams a)
         -> ActivationSource a
         -> IO (Either String a)
evaluate units costF paramList source = do
  let pass = source
          .| forwardPass units costF
          .| sinkList
  let state = def { paramList }
  result <- runAppSConduit pass state
  return $ mapRight (cost . snd) result
