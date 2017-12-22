module School.Unit.CostParams
( CostParams(..) ) where

data CostParams = BatchClassTarget [Int]
                | NoCostParams deriving (Eq, Show)
