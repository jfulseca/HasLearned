module School.Utils.Maybe
( maybeToMonad ) where

import Control.Monad.Except (MonadError(..))
import School.Types.LiftResult (LiftResult(..))

maybeToMonad :: (LiftResult m, MonadError e m)
             => e -> Maybe a -> m a
maybeToMonad e = maybe (throwError e) (liftResult . Right)
