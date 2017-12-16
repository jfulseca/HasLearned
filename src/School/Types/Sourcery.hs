module School.Types.Sourcery
( Sourcery
, liftResumable
, liftSourcery
) where

import Conduit (($$+), (.|), ConduitM, ResumableSource, runConduit, sinkNull, takeC)
import Data.Void (Void)

type Sourcery i m r = ConduitM i Void m r
                   -> m r

liftSourcery :: (Monad m)
             => ConduitM () o m ()
             -> Sourcery o m ()
liftSourcery source sink =
  runConduit $ source .| sink

liftResumable :: (Monad m)
              => ConduitM () o m ()
              -> m (ResumableSource m o)
liftResumable source = do
  (resumable, _) <- source $$+ takeC 0 .| sinkNull
  return resumable
