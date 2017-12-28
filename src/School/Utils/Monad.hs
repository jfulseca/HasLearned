module School.Utils.Monad
( headMonad
, maybeToMonad
) where

import Control.Monad.Except (MonadError(..))
import School.Types.LiftResult (LiftResult(..))

maybeToMonad :: (LiftResult m, MonadError e m)
             => e -> Maybe a -> m a
maybeToMonad e = maybe (throwError e) (liftResult . Right)

headMonad :: (LiftResult m, MonadError e m)
          => [a] -> e -> m a
headMonad [] = throwError
headMonad xs = const . liftResult . Right . head $ xs
