module School.Utils.Tuple
( snd3
, trd3
) where

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
