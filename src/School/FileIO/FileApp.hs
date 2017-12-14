{-# LANGUAGE TypeFamilies #-}

module School.FileIO.FileApp
( FileApp(..)
, fileApp
) where

import Conduit (ConduitM)
import Data.Void (Void)
import School.FileIO.AppIO (AppIO, runConduitInAppIO)
import School.App.AppS (AppS)

class FileApp options where
  data FAParams options :: *
  scan :: options -> AppIO (FAParams options)
  prepare :: (FAParams options)
          -> ConduitM () Void (AppS a) ()

fileApp :: (FileApp o) => o -> AppIO ()
fileApp options = do
  params <- scan options
  let pipeLine = prepare params 
  runConduitInAppIO pipeLine

