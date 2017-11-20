module HML.Aux
( doubleSize
, errorContext
, printConduit
, printVar
, showIO
, sl
) where

import Conduit (ConduitM, mapC, peekC)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (either)
import Debug.Trace (trace, traceIO)
import Foreign.Storable (sizeOf)

sl :: (Show a) => a -> String
sl a = (show a) ++ " "

showIO :: (MonadIO m) => String -> m ()
showIO = liftIO . traceIO

doubleSize :: Int
doubleSize = sizeOf (0 :: Double)

errorContext :: String
             -> Maybe String
             -> Either String a
             -> Either String a
errorContext context suggestion =
  let sugg = maybe "" ((++) " | ") suggestion in
  either (\s -> Left $ context ++ ": " ++ s ++ sugg)
         (Right . id)

printConduit :: (MonadIO m, Show a)
             => String
             -> ConduitM a a m ()
printConduit str = do
  next <- peekC
  liftIO $ putStrLn (str ++ ": " ++ (show next))  
  mapC id

printVar :: (Show a) => String -> a -> a
printVar str x = trace (str ++ ": " ++ (show x)) x
