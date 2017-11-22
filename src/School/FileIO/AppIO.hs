module School.FileIO.AppIO
( Confirmer
, AppIO
, ConduitAppIO
, appIOFail
, confirmAtom
, liftAppIO
, maybeToAppIO
, runAppIO
) where

import Conduit (ConduitM, ResourceT, mapM_C, runConduitRes)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (FromByteString, parser)
import Data.Void (Void)

type AppIO = ResourceT (ExceptT String IO)

type ConduitAppIO = ConduitM ()
                             Void
                             AppIO
                             ()

runAppIO :: ConduitM () Void AppIO a -> IO (Either String a)
runAppIO = runExceptT . runConduitRes

liftAppIO ::Either String a -> AppIO a
liftAppIO = lift . ExceptT . return

appIOFail :: String -> ConduitM i o AppIO ()
appIOFail e = mapM_C (\_ -> liftAppIO. Left $ e)

type Confirmer = ConduitM ByteString
                          ByteString
                          AppIO
                          ()

confirmAtom :: (Eq a, FromByteString a, Show a)
             => (a -> Bool) -> Confirmer
confirmAtom atom = mapM_C (checkAtom atom)

checkAtom :: (Eq a, FromByteString a, Show a)
          => (a -> Bool) -> ByteString -> AppIO ()
checkAtom check bytes = do
  let parseResult = atomParser check bytes
  liftAppIO parseResult
  where
    atomParser c b = do
      atom <- parseOnly parser b
      if c atom
        then Right ()
        else Left $ msg b
    msg b = "Parser gave unexpected "
         ++ "result " ++ (show b)

maybeToAppIO :: String -> Maybe a -> AppIO a
maybeToAppIO msg =
  maybe (liftAppIO . Left $ msg) (liftAppIO . Right)
