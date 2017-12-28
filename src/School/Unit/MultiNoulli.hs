{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Unit.MultiNoulli
( multiNoulli ) where

import Conduit ((.|), ConduitM, MonadResource, ZipSource(..),
                getZipSource, mapC, mapM_C, mapMC)
import Control.Monad.Except (MonadError, throwError)
import Numeric.LinearAlgebra (Container, Element, Matrix, Vector, assoc, cols, fromRows,
                              rows, takeColumns, toColumns, toList, toLists)
import School.FileIO.FileHeader (FileHeader)
import School.FileIO.IntListSource (intListSource)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult)
import School.Types.Slinky (Slinky(..), slinkyAppend, slinkySingleton)
import School.Unit.CostParams (CostParams(..))
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.CostFunction (SetupCost, CostFunction(..), defSetupCost)

toTarget :: (Element a, RealFrac a)
         => Vector a
         -> CostParams
toTarget =
  BatchClassTarget . map round . toList

compute :: (Element a, Fractional a, Num a)
        => UnitActivation a
        -> Slinky CostParams
        -> Either Error a
compute (BatchActivation input)
        (SNode (BatchClassTarget target) _) =
  let factor = (-1) / (fromIntegral . rows $ input)
  in Right
   . (*factor)
   . sum
   . zipWith (flip (!!)) target
   . toLists
   $ input
compute _ _ = Left "MultiNoulli expects batch activation and batch class target"

deriv :: (Container Vector a, Fractional a, Num a)
      => UnitActivation a
      -> Slinky CostParams
      -> Either Error (UnitGradient a)
deriv (BatchActivation input)
      (SNode (BatchClassTarget target) _) =
  let factor = (-1) / (fromIntegral . rows $ input)
      c = cols input
  in Right
   . BatchGradient
   . fromRows
   . map (\idx -> assoc c 0 [(idx, factor)])
   $ target
deriv _ _ = Left "MultiNoulli expects batch activation and batch class target"

prepare :: (Element a, RealFrac a, MonadError Error m)
        => Maybe FilePath
        -> ConduitM (ForwardStack a)
                    (ForwardStack a)
                    m
                    ()
prepare Nothing = mapMC $ \(activations, cParams) ->
  case activations of
    [BatchActivation input] -> do
      let c = cols input
      let activation = takeColumns (c - 1) input
      let newParams = toTarget . last . toColumns $ input
      return ([BatchActivation activation], slinkyAppend newParams cParams)
    _ -> throwError "MultiNoulli setup expects single batch activation"
prepare _ = mapC id

setupError :: (MonadError Error m) => ConduitM () (ForwardStack a) m ()
setupError =
  mapM_C . const $ throwError "Need both filepath and file header"

appendTarget :: CostParams -> Matrix a -> ForwardStack a
appendTarget costParams =
  flip (,) (slinkySingleton costParams) . pure . BatchActivation

setup :: (LiftResult m, MonadError Error m, MonadResource m)
      => Maybe FilePath
      -> Maybe FileHeader
      -> SetupCost a m
setup Nothing Nothing source = defSetupCost source
setup Nothing (Just _) _ = setupError
setup (Just _) Nothing _ = setupError
setup (Just path) (Just header) matrixSource =
  getZipSource $ targetZipSource <*> ZipSource matrixSource
  where targetSource = intListSource header path
                    .| mapC BatchClassTarget
                    .| mapMC (return . appendTarget)
        targetZipSource = ZipSource targetSource


multiNoulli :: ( Container Vector a, LiftResult m, RealFrac a
               , MonadError Error m, MonadResource m )
            => Maybe FilePath
            -> Maybe FileHeader
            -> CostFunction a m
multiNoulli path header = CostFunction { computeCost = compute
                                       , derivCost = deriv
                                       , prepareCost = prepare path
                                       , setupCost = setup path header
                                       }
