{-# LANGUAGE NamedFieldPuns #-}

module School.Evaluate.Evaluate
( evaluate ) where

import Conduit ((.|), await, runConduit)
import School.Train.AppTrain (runAppTrain)
import School.Train.ForwardPass (forwardPass)
import School.Train.TrainState (TrainState(..), def)
import School.Types.Error (Error)
import School.Types.PingPong (PingPong)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitForward (ForwardSource)
import School.Unit.UnitParams (UnitParams)
import School.Utils.Tuple (trd3)

eitherMaybe :: Error
            -> (b -> c)
            -> Either Error ((Maybe b), d)
            -> Either Error c
eitherMaybe _ _ (Left e) = Left e
eitherMaybe e _ (Right (Nothing, _)) = Left e
eitherMaybe _ f (Right ((Just x), _)) = Right . f $ x

evaluate :: (Num a)
         => [Unit a]
         -> CostFunction a
         -> PingPong (UnitParams a)
         -> ForwardSource a
         -> IO (Either String a)
evaluate units costF paramList source = do
  let pass = source
          .| forwardPass units costF
          .| await
  let state = def { paramList }
  result <- runAppTrain state $ runConduit pass
  return $ eitherMaybe "No data obtained" trd3 result
