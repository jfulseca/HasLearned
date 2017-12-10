module School.Train.GradientDescent
( gradientDescent ) where

import Conduit ((.|), ConduitM, mapMC, sinkNull, takeWhileC)
import Control.Monad.State.Lazy (get)
import Data.Maybe (isJust)
import School.App.AppS (AppS, runAppSConduit)
import School.Train.GradientDescentPass (gradientDescentPass)
import School.Train.IterationHandler (IterationHandler(..))
import School.Train.UpdateParams (UpdateParams)
import School.Train.StoppingCondition (StoppingCondition(..))
import School.Train.TrainState (TrainState)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)
import School.Unit.UnitGradient (UnitGradient)
import School.Utils.Either (mapRight)

stopping :: StoppingCondition a
         -> ConduitM (UnitGradient a)
                     (Maybe (UnitGradient a))
                     (AppS a)
                     ()
stopping condition = mapMC $ \input -> do
  state <- get
  if (runCondition condition) state
    then return Nothing
    else return . Just $ input

gradientDescent :: ConduitM ()
                            (UnitActivation a)
                            (AppS a)
                            ()
                -> [Unit a]
                -> CostFunction a
                -> UpdateParams a
                -> StoppingCondition a
                -> IterationHandler a (UnitGradient a)
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
  let pass = gradientDescentPass units cost update
  let iterations = source
                .| pass
                .| runHandler handler
                .| stopping condition
                .| takeWhileC isJust
                .| sinkNull
  result <- runAppSConduit iterations $ initState
  return $ mapRight snd result
