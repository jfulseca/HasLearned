{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module School.Types.LiftResult
( LiftResult(..) ) where

import Data.Attoparsec.ByteString (Parser)
import School.Types.Error (Error)

class (Monad m) => LiftResult m where
  liftResult :: Either Error a -> m a

instance LiftResult Parser where
  liftResult (Right x) = return x
  liftResult (Left e) = fail e
