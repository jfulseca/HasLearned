{-# LANGUAGE TemplateHaskell #-}

module School.
( ) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH

:: TestTree
= $(testGroupGenerator)