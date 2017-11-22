{-# LANGUAGE NamedFieldPuns #-}

module School.Unit.Affine
( affine ) where

import Numeric.LinearAlgebra ((<>), R, size, tr')
import School.Unit.Constituents (Constituents(..))
import School.Unit.Unit (Unit(..), UnitOutput)
import School.Utils.LinearAlgebra ((+>), oneVector)

affine :: Unit Double
affine = Unit
  { deriv = affineDeriv
  , op = affineOp
  }

affineOp :: Constituents R -> UnitOutput R
affineOp AffineConstituents { biasVector, inputMatrix, weightMatrix } =
  (inputMatrix <> tr' weightMatrix) +> biasVector

affineDeriv :: Constituents R ->
               UnitOutput R ->
               Constituents R
affineDeriv AffineConstituents { biasVector, inputMatrix, weightMatrix } grad =
  AffineConstituents { biasVector=biasVector', inputMatrix=inputMatrix', weightMatrix=weightMatrix' }
  where biasVector' = oneVector (size biasVector)
        inputMatrix' = grad <> weightMatrix
        weightMatrix' = tr' grad <> inputMatrix
