{-# LANGUAGE TypeFamilies #-}

module School.FileIO.FileApp
( FileApp(..)
, fileApp
) where

import Conduit (ConduitM, runConduit)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO)

class FileApp options where
  data FAParams options :: *
  scan :: options -> AppIO (FAParams options)
  prepare :: (FAParams options)
          -> ConduitM () Void AppIO ()

fileApp :: (FileApp o) => o -> AppIO ()
fileApp options = do
  params <- scan options
  let pipeLine = prepare params 
  runConduit pipeLine
