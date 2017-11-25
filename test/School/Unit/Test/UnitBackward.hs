{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module School.Unit.Test.UnitBackward
( unitBackwardTest) where

import Conduit ((.|), ConduitM, runConduit, sinkList, yield)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (runStateT)
import Data.Either (isLeft)
import Numeric.LinearAlgebra (R, ident)
import Data.Void (Void)
import School.TestUtils (randomAffineParams, randomMatrix)
import School.Train.AppTrain (AppTrain)
import School.Train.TrainState (TrainState(..))
import School.Types.FloatEq ((~=))
import School.Types.PingPong (toPingPong)
import School.Unit.Affine (affine)
import School.Unit.RecLin (recLin)
import School.Unit.Unit (Unit(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitBackward
import School.Unit.UnitGradient (UnitGradient(..), isGradientFail)
import School.Unit.UnitParams (UnitParams(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Test.QuickCheck.Monadic (assert, monadicIO)

runTest :: ConduitM ()
                    Void
                    (AppTrain R)
                    a
        -> TrainState Double
        -> Either String (a, TrainState R)
runTest conduit state = runExcept $
  runStateT (runConduit conduit) state

prop_affine_input_fail :: Bool
prop_affine_input_fail = let
  backward = unitBackward affine
  acts = [ApplyFail "init"]
  inGrad = BatchGradient $ ident 1
  network =  yield (acts, inGrad)
          .| backward
          .| sinkList
  paramList = toPingPong [EmptyParams]
  paramDerivs = []
  initState = TrainState { paramDerivs, paramList }
  result = runTest network initState
  in isLeft result

prop_affine_gradient_fail :: Bool
prop_affine_gradient_fail = let
  backward = unitBackward affine
  acts = [BatchActivation $ ident 1]
  inGrad = GradientFail "init"
  network =  yield (acts, inGrad)
          .| backward
          .| sinkList
  paramList = toPingPong [EmptyParams]
  paramDerivs = []
  initState = TrainState { paramDerivs, paramList }
  result = runTest network initState
  in isLeft result

prop_affine_param_fail :: Positive Int -> Positive Int -> Property
prop_affine_param_fail (Positive fSize) (Positive oSize) = monadicIO $ do
  let backward = unitBackward affine
  actMat <- liftIO $ randomMatrix oSize fSize
  gradMat <- liftIO $ randomMatrix oSize fSize
  let acts = [BatchActivation actMat]
  let fStack = (acts, BatchGradient gradMat)
  let network =  yield fStack
              .| backward
              .| sinkList
  let paramList = toPingPong [EmptyParams]
  let initState = TrainState { paramDerivs = []
                             , paramList }
  let result = runTest network initState
  either (const . assert $ False)
         (\(stack, _) -> assert $ isGradientFail (snd . head $ stack))
         result

prop_reclin_gradient :: Positive Int -> Positive Int -> Property
prop_reclin_gradient (Positive bSize) (Positive fSize) = monadicIO $ do
  let backward = unitBackward recLin
  actMat <- liftIO $ randomMatrix bSize fSize
  let input = BatchActivation actMat
  gradMat <- liftIO $ randomMatrix bSize fSize
  let inGrad = BatchGradient gradMat
  let fStack = ([input], inGrad)
  let network =  yield fStack
              .| backward
              .| sinkList
  let paramList = toPingPong [EmptyParams]
  let initState = TrainState { paramDerivs = []
                             , paramList }
  let result = runTest network initState
  let (check, _) = deriv recLin EmptyParams inGrad input
  either (const . assert $ False)
         (\(stack, _) -> assert $ check ~= (snd . head $ stack))
         result

prop_affine_derivs :: Positive Int -> Positive Int -> Positive Int -> Property
prop_affine_derivs (Positive bSize) (Positive fSize) (Positive oSize) = monadicIO $ do
  let backward = unitBackward affine
  actMat <- liftIO $ randomMatrix bSize fSize
  let input = BatchActivation actMat
  gradMat <- liftIO $ randomMatrix bSize oSize
  let inGrad = BatchGradient gradMat
  let fStack = ([input], inGrad)
  let network =  yield fStack
              .| backward
              .| sinkList
  params <- liftIO $ randomAffineParams fSize oSize
  let paramList = toPingPong [params]
  let initState = TrainState { paramDerivs = []
                             , paramList }
  let result = runTest network initState
  let (_, check) = deriv affine params inGrad input
  either (const . assert $ False)
         (\(_, TrainState { paramDerivs }) -> assert $ check ~= (head paramDerivs))
         result

prop_deriv_aff_rl_aff_rl :: Positive Int
                         -> Positive Int
                         -> Positive Int
                         -> Positive Int
                         -> Property
prop_deriv_aff_rl_aff_rl (Positive b)
                         (Positive f)
                         (Positive h)
                         (Positive o) = monadicIO $ do
  inMat <- liftIO $ randomMatrix b f
  act1Mat <- liftIO $ randomMatrix b h
  act2Mat <- liftIO $ randomMatrix b h
  act3Mat <- liftIO $ randomMatrix b o
  let acts = BatchActivation <$> [ inMat
                                 , act1Mat
                                 , act2Mat
                                 , act3Mat
                                 ]
  gradMat <- liftIO $ randomMatrix b o
  let inGrad = BatchGradient gradMat
  let network =  yield (acts, inGrad)
              .| unitBackward recLin
              .| unitBackward affine
              .| unitBackward recLin
              .| unitBackward affine
              .| sinkList
  params1 <- liftIO $ randomAffineParams f h
  params2 <- liftIO $ randomAffineParams h o
  let paramList = toPingPong [ EmptyParams
                             , params2
                             , EmptyParams
                             , params1
                             ]
  let initState = TrainState { paramDerivs = []
                             , paramList }
  let result = runTest network initState
  let (grad1, _) = deriv recLin EmptyParams inGrad (last acts)
  let (grad2, dParams2) = deriv affine params2 grad1 (acts!!2)
  let (grad3, _) = deriv recLin EmptyParams grad2 (acts!!1)
  let (_, dParams1) = deriv affine params1 grad3 (head acts)
  let check = [ dParams1
              , EmptyParams
              , dParams2
              , EmptyParams
              ]
  either (const . assert $ False)
         (\(_, TrainState { paramDerivs }) ->
           assert $ check ~= paramDerivs)
         result

unitBackwardTest :: TestTree
unitBackwardTest = $(testGroupGenerator)
