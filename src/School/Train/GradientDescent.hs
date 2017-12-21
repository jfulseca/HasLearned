module School.Train.GradientDescent
( gradientDescent ) where

import Conduit ((.|), ConduitM, mapC, mapMC, sinkNull, takeWhileC)
import Control.Monad.State.Lazy (get)
import Data.Maybe (isJust)
import School.Train.AppTrain (AppTrain, runAppTrain)
import School.FileIO.MatrixSourcery (MatrixSourcery)
import School.Train.GradientDescentPass (gradientDescentPass)
import School.Train.IterationHandler (IterationHandler(..))
import School.Train.UpdateParams (UpdateParams)
import School.Train.StoppingCondition (StoppingCondition(..))
import School.Train.TrainState (TrainState)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitBackward (BackwardStack)

stopping :: StoppingCondition a
         -> ConduitM (BackwardStack a)
                     (Maybe ())
                     (AppTrain a)
                     ()
stopping condition = mapMC $ \_ -> do
  state <- get
  if (runCondition condition) state
    then return Nothing
    else return (Just ())

gradientDescent :: MatrixSourcery (AppTrain a) a ()
                -> [Unit a]
                -> CostFunction a
                -> UpdateParams a
                -> StoppingCondition a
                -> IterationHandler a
                -> TrainState a
                -> IO (Either String
                              (TrainState a))
gradientDescent sourcerer
                units
                cost
                update
                condition
                handler
                initState = do
  let sink = mapC BatchActivation
          .| gradientDescentPass units cost update
          .| runHandler handler
          .| stopping condition
          .| takeWhileC isJust
          .| sinkNull
  result <- runAppTrain initState $
    sourcerer sink
  return $ fmap snd result
