module School.App.AppIO
( AppIO
, liftAppIO
, runAppIO
, runConduitInAppIO
) where

import Conduit (ConduitM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Void (Void)
import School.App.AppS (AppS, runAppSConduitDefState)

type AppIO a = ExceptT String IO a

liftAppIO :: Either String a -> AppIO a
liftAppIO = ExceptT . return

runAppIO :: AppIO a -> IO (Either String a)
runAppIO = runExceptT

runConduitInAppIO :: ConduitM ()
                              Void
                              (AppS Int)
                              b
                  -> AppIO b
runConduitInAppIO = ExceptT . runAppSConduitDefState
