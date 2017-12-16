module School.Types.Sourcery
( Sourcery
, liftSourcery
, sourcery
) where

import Conduit (($$+-), (.|), ConduitM, ResumableSource, runConduit)
import Data.Void (Void)

type Sourcery i m r = ConduitM i Void m r
                   -> m r

liftSourcery :: (Monad m)
             => ConduitM () o m ()
             -> Sourcery o m ()
liftSourcery source sink = do
  runConduit $ source .| sink

sourcery :: (Monad m)
         => ResumableSource m a
         -> ConduitM a b m ()
         -> Sourcery b m r
sourcery resumable conduit sink =
  resumable $$+- (conduit .| sink)
