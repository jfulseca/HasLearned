{-# LANGUAGE TemplateHaskell #-}

module School.Evaluate.Test.Evaluate
( evaluateTest ) where

import Conduit (liftIO, yield)
import School.Evaluate.Evaluate
import School.TestUtils (addClasses, assertRight, fromRight, randomAffineParams,
                         randomMatrix, randomNNInts, unitCorrect)
import School.Types.FloatEq ((~=))
import School.Types.PingPong (pingPongSingleton, toPingPong)
import School.Types.Slinky (Slinky(..))
import School.Unit.Affine (affine)
import School.Unit.LogSoftMax (logSoftMax)
import School.Unit.RecLin (recLin)
import School.Unit.MultiNoulli (multiNoulli)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty.TH
import Test.QuickCheck.Monadic (monadicIO, pre)

prop_single_layer :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_single_layer (Positive b) (Positive f) (Positive h) (Positive c) = monadicIO $ do
  pre $ c > 1
  input <- liftIO $ randomMatrix b f
  p1 <- liftIO $ randomAffineParams f h
  p2 <- liftIO $ randomAffineParams h c
  classes <- liftIO $ randomNNInts (c - 1) b
  let units = [affine, recLin, affine, logSoftMax]
  let params = [p1, EmptyParams, p2, EmptyParams]
  let paramList = fromRight (pingPongSingleton EmptyParams)
                            (toPingPong params)
  let source = yield ([BatchActivation $ addClasses classes input], SNil)
  result <- liftIO $ evaluate units
                              (multiNoulli Nothing Nothing)
                              paramList
                              source
  assertRight (> (-1)) result

prop_correct :: Positive Int -> Positive Int -> Property
prop_correct (Positive b) (Positive c) = monadicIO $ do
  input <- liftIO $ randomMatrix b c
  classes <- liftIO $ randomNNInts (c - 1) b
  let paramList = pingPongSingleton EmptyParams
  let source = yield ([BatchActivation $ addClasses classes input], SNil)
  result <- liftIO $ evaluate [unitCorrect classes]
                              (multiNoulli Nothing Nothing)
                              paramList
                              source
  assertRight (~= (-1)) result

evaluateTest :: TestTree
evaluateTest = $(testGroupGenerator)
