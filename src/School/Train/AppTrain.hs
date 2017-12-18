{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module School.Train.AppTrain
( AppTrain
, maybeToAppTrain
, runAppTrain
, runAppTrainPure
) where 

import Conduit (ResourceT, runResourceT)
import Control.Monad.Except (ExceptT(..), throwError, runExceptT)
import Control.Monad.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import School.Train.TrainState (TrainState, def)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

type AppTrain a = ResourceT (StateT (TrainState a)
                                    (ExceptT Error
                                             IO))

instance LiftResult (AppTrain a)
  where liftResult = lift . lift . ExceptT . return


runAppTrain :: TrainState a
            -> AppTrain a b
            -> IO (Either String (b, TrainState a))
runAppTrain state app = runExceptT $
  runStateT (runResourceT app) state

runAppTrainPure :: (Num a)
                => AppTrain a b
                -> IO (Either String b)
runAppTrainPure app =
  (fst <$>) <$> (runAppTrain def app)

maybeToAppTrain :: String -> Maybe b -> AppTrain a b
maybeToAppTrain msg =
  maybe (throwError msg) (liftResult . Right)
