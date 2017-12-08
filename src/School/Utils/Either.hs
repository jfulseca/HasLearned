module School.Utils.Either
( mapRight ) where

import Data.Either (either)

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)
