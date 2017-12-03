{-# LANGUAGE NamedFieldPuns #-}

module School.Types.PingPong
( PingPong
, getPingPong
, pingPongList
, pingPongSingleton
, reversePingPong
, toPingPong
) where

data PingPong a =
  PingPong { pingPongBackward :: Bool
           , pingPongIdx :: Int
           , pingPongLength :: Int
           , pingPongList :: [a]
           } deriving (Eq, Show)

toPingPong :: [a] -> Either String (PingPong a)
toPingPong [] = Left "Cannot have empty PingPong list"
toPingPong list = Right $
  PingPong { pingPongBackward = False
           , pingPongIdx = 0
           , pingPongLength = length list
           , pingPongList = list
           }

getPingPong :: PingPong a -> (a, PingPong a)
getPingPong p =
  let idx = pingPongIdx p
      list = pingPongList p
  in if (pingPongBackward p)
    then let
      goForward = idx < 1
      newIdx = if goForward then idx else idx - 1
      newPingPong = p { pingPongBackward = not goForward
                      , pingPongIdx = newIdx
                      }
      in (list!!idx, newPingPong)
    else let
      goBackward = idx + 1 >= pingPongLength p
      newIdx = if goBackward then idx else idx + 1
      newPingPong = p { pingPongBackward = goBackward
                      , pingPongIdx = newIdx
                      }
      in (list!!idx, newPingPong)

pingPongSingleton :: a -> PingPong a
pingPongSingleton element =
  PingPong { pingPongBackward = False
           , pingPongIdx = 0
           , pingPongLength = 1
           , pingPongList = [element]
           }

reversePingPong :: PingPong a -> PingPong a
reversePingPong p@PingPong{ pingPongLength } =
  p { pingPongBackward = True
    , pingPongIdx = pingPongLength - 1
    }
