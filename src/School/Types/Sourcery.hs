module School.Types.Sourcery
( Sourcery
, liftsourcery
, sourcery
) where

import Conduit (($$+-), (.|), ConduitM, ResourceT,
                ResumableSource, runConduitRes)
import Data.Void (Void)

type Sourcery i m r = ConduitM i Void m r
                   -> m r

liftSourcery :: ConduitM () o (ResourceT m) ()
             -> Sourcery o m ()
liftSourcery source sink =
  runConduitRes $ source .| sink

sourcery :: (Monad m)
         => ResumableSource m a
         -> ConduitM a b m ()
         -> Sourcery b m r
sourcery resumable conduit sink =
  resumable $$+- (conduit .| sink)
