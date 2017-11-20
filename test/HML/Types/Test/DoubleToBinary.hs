{-# LANGUAGE TemplateHaskell #-}

module HML.Types.Test.DoubleToBinary
( doubleToBinaryTest ) where

import Data.Either (either)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import HML.Types.DoubleToBinary
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

prop_isomorphic :: Double -> Bool
prop_isomorphic d = d == d' where
  binary = runPut $ doubleToBinary d
  converted = runGet doubleFromBinary binary
  d' = either (\_ -> d + 1) id converted

doubleToBinaryTest :: TestTree
doubleToBinaryTest = $(testGroupGenerator)
