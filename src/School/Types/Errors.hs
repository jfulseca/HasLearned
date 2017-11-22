module School.Types.Errors
( errorContext ) where

errorContext :: String
             -> Maybe String
             -> Either String a
             -> Either String a
errorContext context suggestion =
  let sugg = maybe "" (++ " - ") suggestion
  in either (\s -> Left $ context ++ " : " ++ s ++ sugg)
            (Right . id)
