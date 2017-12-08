module School.App.AppS
( AppS
, FullConduitAppS
, liftAppS
, maybeToAppS
, runAppSConduit
, runAppSConduitDefState
, throw
, throwConduit
) where 

import Conduit (ConduitM, ResourceT, runConduitRes)
import Control.Monad.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Void (Void)
import School.Train.TrainState (TrainState, emptyTrainState)

type AppS a = ResourceT (StateT (TrainState a)
                                (ExceptT String
                                         IO))

liftAppS :: Either String b -> AppS a b
liftAppS = lift . lift . ExceptT . return

maybeToAppS :: String -> Maybe b -> AppS a b
maybeToAppS msg =
  maybe (liftAppS . Left $ msg) (liftAppS . Right)

type FullConduitAppS a = ConduitM ()
                                  Void
                                  (AppS a)
                                  ()

runAppSConduit :: ConduitM ()
                           Void
                           (AppS a)
                           b
               -> (TrainState a)
               -> IO (Either String (b, TrainState a))
runAppSConduit conduit state = runExceptT $
  runStateT (runConduitRes conduit) state

runAppSConduitDefState :: (Num a)
                       => ConduitM ()
                                   Void
                                   (AppS a)
                                   b
                       -> IO (Either String b)
runAppSConduitDefState conduit = do
  result <- runAppSConduit conduit
                           emptyTrainState
  return $ either Left
                  (\(result', _) -> Right result')
                  result

throw :: String -> AppS a b
throw = liftAppS . Left

throwConduit :: String -> ConduitM i o (AppS a) ()
throwConduit = lift . liftAppS . Left

{-
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

-}
