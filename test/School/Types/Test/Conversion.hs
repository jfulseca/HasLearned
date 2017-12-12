{-# LANGUAGE TemplateHaskell #-}

module School.Types.Test.Conversion
( conversionTest ) where

import Data.Either (either)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import School.Types.Decoding
import School.Types.Encoding
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH

prop_double_isomorphic :: Double -> Bool
prop_double_isomorphic d =
  either (const False)
         (==d)
         d'
  where d' = binToDouble . doubleToBin $ d

prop_encode_isomorphic :: Double -> Bool
prop_encode_isomorphic d = d == d' where
  binary = runPut $ putDouble d
  converted = runGet getDouble binary
  d' = either (\_ -> d + 1) id converted

conversionTest :: TestTree
conversionTest = $(testGroupGenerator)
