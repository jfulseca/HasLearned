module School.Train.GradientDescent
( gradientDescent ) where

import Conduit ((.|), ConduitM, mapMC, sinkNull, takeWhileC)
import Control.Monad.State.Lazy (get)
import Data.Either (either)
import Data.Maybe (isJust)
import School.Train.AppTrain (AppTrain, runTrainConduit)
import School.Train.GradientDescentPass (gradientDescentPass)
import School.Train.IterationHandler (IterationHandler)
import School.Train.UpdateParams (UpdateParams)
import School.Train.StoppingCondition (StoppingCondition)
import School.Train.TrainState (TrainState)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)
import School.Unit.UnitGradient (UnitGradient)

stopping :: StoppingCondition a
         -> ConduitM (UnitGradient a)
                     (Maybe (UnitGradient a))
                     (AppTrain a)
                     ()
stopping condition = mapMC $ \input -> do
  state <- get
  if condition state
    then return Nothing
    else return . Just $ input

gradientDescent :: ConduitM ()
                            (UnitActivation a)
                            (AppTrain a)
                            ()
                -> [Unit a]
                -> CostFunction a
                -> UpdateParams a
                -> StoppingCondition a
                -> IterationHandler a (UnitGradient a)
                -> TrainState a
                -> Either String
                          (TrainState a)
gradientDescent source
                units
                cost
                update
                condition
                handler
                initState = 
  let pass = gradientDescentPass units cost update
      iterations = source
                .| pass
                .| handler
                .| stopping condition
                .| takeWhileC isJust
                .| sinkNull
      result = runTrainConduit iterations $ initState
  in either Left
            (\(_, state) -> Right state)
            result
