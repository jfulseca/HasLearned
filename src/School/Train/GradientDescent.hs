module School.Train.GradientDescent
( gradientDescent ) where

import Conduit ((.|), ConduitM, mapMC, runConduit, sinkNull, takeWhileC)
import Control.Monad.State.Lazy (get)
import Data.Maybe (isJust)
import School.Train.AppTrain (AppTrain, runAppTrain)
import School.FileIO.MatrixSource (MatrixSource)
import School.Train.GradientDescentPass (gradientDescentPass)
import School.Train.IterationHandler (IterationHandler(..))
import School.Train.UpdateParams (UpdateParams)
import School.Train.StoppingCondition (StoppingCondition(..))
import School.Train.TrainState (TrainState)
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.Unit (Unit)
import School.Unit.UnitBackward (BackwardStack)

stopping :: StoppingCondition a
         -> ConduitM (BackwardStack a)
                     (Maybe ())
                     (AppTrain a)
                     ()
stopping condition = mapMC $ \_ -> do
  state <- get
  if runCondition condition state
    then return Nothing
    else return (Just ())

gradientDescent :: MatrixSource (AppTrain a) a
                -> [Unit a]
                -> CostFunction a (AppTrain a)
                -> UpdateParams a
                -> StoppingCondition a
                -> IterationHandler a
                -> TrainState a
                -> IO (Either String
                              (TrainState a))
gradientDescent source
                units
                cost
                update
                condition
                handler
                initState = do
  let pass = setupCost cost source
          .| gradientDescentPass units cost update
          .| runHandler handler
          .| stopping condition
          .| takeWhileC isJust
          .| sinkNull
  result <- runAppTrain initState . runConduit $ pass
  return $ snd <$> result
