module School.Types.Slinky
( Slinky(..)
, slinkyPrepend
, slinkySingleton
, toSlinky
) where

data Slinky a =
   SNode a (Slinky a)
 | SNil deriving (Show)

slinkySingleton :: a -> Slinky a
slinkySingleton val = SNode val SNil

slinkyPrepend :: a
              -> Slinky a
              -> Slinky a
slinkyPrepend val SNil =
  slinkySingleton val
slinkyPrepend val (SNode first rest)
  = SNode val (SNode first rest)

toSlinky :: [a] -> Slinky a
toSlinky = foldl (flip slinkyPrepend) SNil
