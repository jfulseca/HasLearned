module School.Types.LiftResult
( LiftResult(..) ) where

import School.Types.Error (Error)

class (Monad m) => LiftResult m where
  liftResult :: Either Error a -> m a
