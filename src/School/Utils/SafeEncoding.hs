module School.Utils.SafeEncoding
( safeStdEncodings ) where

import System.IO

makeEncodingSafe :: Handle -> IO ()
makeEncodingSafe h = do
  mEnc <- hGetEncoding h
  case mEnc of
    Nothing -> return ()
    Just enc -> mkTextEncoding
      ((takeWhile (/= '/') $ show enc) ++ "//TRANSLIT") >>=
        hSetEncoding h

safeStdEncodings :: IO ()
safeStdEncodings =
 mapM_ makeEncodingSafe [stdout, stdin, stderr]
