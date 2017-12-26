{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module School.FileIO.ConduitHeader
( HeaderSink
, conduitHeader
) where

import Conduit (ConduitM, mapMC)
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Data.Attoparsec.ByteString (IResult(..), Result, parse)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (FromByteString(..))
import School.FileIO.FileHeader (FileHeader(..), compatibleHeaders)
import School.Types.Error (Error)
import School.Types.LiftResult (LiftResult(..))

type HeaderSink m =
  ConduitM ByteString ByteString m ()

conduitHeader :: (LiftResult m, MonadError Error m)
              => FileHeader
              -> HeaderSink m
conduitHeader header = mapMC $ \bytes -> do
  let parseResult = parse parser bytes :: Result FileHeader
  case parseResult of
    Done unused parsedHeader -> do
      when (not $ compatibleHeaders header parsedHeader)
           (throwError $ "Parsed header " ++ (show parsedHeader)
                      ++ " not comparible with expected "
                      ++ (show header))
      return unused
    Partial _ -> throwError "Insufficient data to parse header"
    Fail _ context e -> throwError $ "Error " ++ (show e)
                                  ++ if (length context > 0)
                                       then " in context "
                                       else ""
                                  ++ concat context
