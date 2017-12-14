module School.Utils.Either
( eitherToBool
, fromLeft
, fromRight
, isLeft
, isRight
, mapRight
, maybeToEither
) where

import Data.Either (either, isLeft, isRight)

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x _ = x

fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _ = x

eitherToBool :: (b -> Bool)
             -> Either a b
             -> Bool 
eitherToBool _ (Left _) = False
eitherToBool f (Right x) = f x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x Nothing = Left x
maybeToEither _ (Just x) = Right x 
