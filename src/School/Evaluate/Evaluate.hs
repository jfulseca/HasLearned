{-# LANGUAGE NamedFieldPuns #-}

module School.Evaluate.Evaluate
( evaluate ) where

import Conduit ((.|), runConduit, sinkList)
import School.Train.AppTrain (runAppTrain)
import School.Train.ForwardPass (forwardPass)
import School.Train.TrainState (TrainState(..), def)
import School.Types.PingPong (PingPong)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitForward (ForwardSource)
import School.Unit.UnitParams (UnitParams)

evaluate :: (Num a)
         => [Unit a]
         -> CostFunction a
         -> PingPong (UnitParams a)
         -> ForwardSource a
         -> IO (Either String a)
evaluate units costF paramList source = do
  let pass = source
          .| forwardPass units costF
          .| sinkList
  let state = def { paramList }
  result <- runAppTrain state $ runConduit pass
  return $ fmap (cost . snd) result
