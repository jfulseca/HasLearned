{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module School.FileIO.AppIO
( AppIO
, liftResult
, maybeToAppIO
, runAppIO
) where

import Conduit (ResourceT, runResourceT)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

type AppIO = ResourceT (ExceptT Error IO)

instance LiftResult AppIO where
  liftResult = lift . ExceptT . return

runAppIO :: AppIO a -> IO (Either String a)
runAppIO = runExceptT . runResourceT

maybeToAppIO :: Error -> Maybe a -> AppIO a
maybeToAppIO e =
  maybe (throwError e) (liftResult . Right)
