module School.Utils.Debug
( db
, printConduit
, printVar
, showIO
, sl
) where

import Conduit (ConduitM, mapC, peekC)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Debug.Trace (trace, traceIO)

sl :: (Show a) => a -> String
sl a = (show a) ++ " "

showIO :: (MonadIO m) => String -> m ()
showIO = liftIO . traceIO

printConduit :: (MonadIO m, Show a)
             => String
             -> ConduitM a a m ()
printConduit str = do
  next <- peekC
  liftIO $ putStrLn (str ++ ": " ++ (show next))  
  mapC id

printVar :: (Show a) => String -> a -> a
printVar str x = trace (str ++ ": " ++ (show x)) x

db :: a -> String -> a
db = flip trace
