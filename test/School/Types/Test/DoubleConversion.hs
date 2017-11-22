{-# LANGUAGE TemplateHaskell #-}

module School.Types.Test.DoubleConversion
( doubleToBinaryTest ) where

import Data.Either (either)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import School.Types.DoubleConversion
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

prop_isomorphic :: Double -> Bool
prop_isomorphic d =
  either (const False)
         (==d)
         d'
  where d' = fromBinary . toBinary $ d

prop_encode_isomorphic :: Double -> Bool
prop_encode_isomorphic d = d == d' where
  binary = runPut $ putDouble d
  converted = runGet getDouble binary
  d' = either (\_ -> d + 1) id converted

doubleToBinaryTest :: TestTree
doubleToBinaryTest = $(testGroupGenerator)
