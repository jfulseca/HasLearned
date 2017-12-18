{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module School.App.AppS
( AppS
, ConduitBS
, FullConduitAppS
, liftResult
, maybeToAppS
, runAppS
, runAppSConduit
, runAppSConduitDefState
, runAppSPure
, throwError
, throwConduit
) where 

import Conduit (ConduitM, ResourceT, runConduitRes, runResourceT)
import Control.Monad.Except (ExceptT(..), throwError, runExceptT)
import Control.Monad.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Void (Void)
import School.Train.TrainState (TrainState, def)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))
import School.Utils.Either (mapRight)

type AppS a = ResourceT (StateT (TrainState a)
                                (ExceptT Error
                                         IO))

instance LiftResult (AppS a)
  where liftResult = lift . lift . ExceptT . return


runAppS :: TrainState a
        -> AppS a b
        -> IO (Either String (b, TrainState a))
runAppS state app = runExceptT $
  runStateT (runResourceT app) state

runAppSPure :: (Num a)
            => AppS a b
            -> IO (Either String b)
runAppSPure app =
  (fst <$>) <$> (runAppS def app)

type FullConduitAppS a = ConduitM ()
                                  Void
                                  (AppS a)
                                  ()

runAppSConduit :: ConduitM ()
                           Void
                           (AppS a)
                           b
               -> (TrainState a)
               -> IO (Either String (b, TrainState a))
runAppSConduit conduit state = runExceptT $
  runStateT (runConduitRes conduit) state

runAppSConduitDefState :: (Num a)
                       => ConduitM ()
                                   Void
                                   (AppS a)
                                   b
                       -> IO (Either String b)
runAppSConduitDefState conduit = do
  result <- runAppSConduit conduit
                           def
  return $ mapRight fst result

throw :: String -> AppS a b
throw = liftResult . Left

throwConduit :: String -> ConduitM i o (AppS a) ()
throwConduit = lift . throw

maybeToAppS :: String -> Maybe b -> AppS a b
maybeToAppS msg =
  maybe (throw msg) (liftResult . Right)

type ConduitBS a = ConduitM ByteString ByteString (AppS a) () 
