module School.FileIO.Confirmer
( Confirmer
, confirmAtom
) where

import Conduit (ConduitM, mapM_C)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (FromByteString, parser)
import School.App.AppS (AppS, liftAppS)

type Confirmer a = ConduitM ByteString
                            ByteString
                            (AppS a)
                            ()

confirmAtom :: (Eq b, FromByteString b, Show b)
            => (b -> Bool)
            -> Confirmer a
confirmAtom check = mapM_C $ \bytes -> do
  let parseResult = atomParser check bytes
  liftAppS parseResult
  where
    atomParser c b = do
      atom <- parseOnly parser b
      if c atom
        then Right ()
        else Left $ msg b
    msg b = "Parser gave unexpected "
         ++ "result " ++ (show b)
