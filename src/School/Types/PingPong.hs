module School.Types.PingPong
( PingPong
, toPingPong
) where

type PingPong a = [a]

toPingPong :: [a] -> PingPong a
toPingPong list = backForth ++ (toPingPong list)
  where backForth = list ++ (reverse list)
