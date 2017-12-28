{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.Train.SimpleDescentUpdate
( simpleDescentUpdate ) where

import Numeric.LinearAlgebra (Container, Vector, add, scale)
import School.Train.TrainState (TrainState(..))
import School.Train.UpdateParams (UpdateParams)
import School.Types.PingPong (pingPongList, toPingPong)
import School.Unit.UnitParams (UnitParams(..))


update :: (Container Vector a, Num a)
       => a
       -> UnitParams a
       -> UnitParams a
       -> UnitParams a
update rate
       AffineParams { affineBias = bias
                    , affineWeights = weights}
       AffineParams { affineBias = biasGrad
                    , affineWeights = weightGrad } =
  AffineParams { affineBias, affineWeights } where
    affineBias = add bias (scale (-rate) biasGrad)
    affineWeights = add weights (scale (-rate) weightGrad)
update _ _ _ = EmptyParams

simpleDescentUpdate :: (Container Vector a, Num a)
                    => UpdateParams a
simpleDescentUpdate state@TrainState { paramDerivs
                                     , paramList
                                     , learningRate
                                     } = do
  newParamList <- toPingPong $
         zipWith (update learningRate)
                 (pingPongList paramList)
                 paramDerivs
  return state { paramList = newParamList }
