{-# LANGUAGE NamedFieldPuns #-}

module School.Utils.Random
( randAffineParams ) where

import Numeric.LinearAlgebra ((|>), (><), R)
import System.Random (StdGen, randomR)
import School.Unit.UnitParams (UnitParams(..))

randAffineParams :: Int
                 -> Int
                 -> (R, R)
                 -> StdGen
                 -> (UnitParams R, StdGen)
randAffineParams nIn nOut limits gen =
  let (biasList, gen') = go nOut gen []
      (weightList, gen'') = go (nIn * nOut) gen' []
      go 0 g acc = (acc, g)
      go n g acc = go (n - 1) g' acc' where
        (k, g') = randomR limits g
        acc' = k:acc
      affineBias = nOut |> biasList
      affineWeights = (nOut >< nIn) weightList
      params = AffineParams { affineBias, affineWeights }
  in (params, gen'')

