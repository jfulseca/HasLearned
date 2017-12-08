module School.Unit.CostParams
( CostParams(..)
, LinkedParams(..)
, nextParams
, paramPrepend
, paramSingleton
, toLinkedParams
) where

data CostParams = BatchClassTarget [Int]
                | NoCostParams deriving (Show)

data LinkedParams =
   Node CostParams LinkedParams
 | NoNode deriving (Show)
nextParams :: LinkedParams -> LinkedParams
nextParams (Node _ p) = p
nextParams _ = undefined

paramSingleton :: CostParams -> LinkedParams
paramSingleton params = Node params NoNode

paramPrepend :: CostParams
            -> LinkedParams
            -> LinkedParams
paramPrepend params NoNode = paramSingleton params
paramPrepend params (Node first rest)
  = Node params (Node first rest)

toLinkedParams :: [CostParams] -> LinkedParams
toLinkedParams paramsList =
  foldl (flip paramPrepend) NoNode paramsList
