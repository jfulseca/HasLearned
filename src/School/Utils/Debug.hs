module School.Utils.Debug
( compareDoubleMatrixTrace
, db
, printConduit
, printVar
, showIO
, sl
) where

import Conduit (ConduitM, mapC, peekC)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Debug.Trace (trace, traceIO)
import Numeric.LinearAlgebra (Matrix, R, toLists)
import School.Types.FloatEq (compareDouble)

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

compareDoubleMatrixTrace :: Double
                         -> Matrix R
                         -> Matrix R
                         -> Bool
compareDoubleMatrixTrace prec m1 m2 =
  and $ zipWith (traceCompare prec) l1 l2
  where traceMsg c msg v = if c
           then trace msg v
           else v
        traceCompare p d1 d2 =
          let equal = compareDouble p d1 d2
          in traceMsg (not equal) ("UNEQ " ++ (show d1) ++ " " ++(show d2)) equal
        l1 = concat . toLists $ m1
        l2 = concat . toLists $ m2

