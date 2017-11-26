module School.Train.GradientDescent
( gradientDescent ) where

import Conduit ((.|), ConduitM, await)
import Control.Monad.State.Lazy (get)
import Data.Either (either)
import Data.Void (Void)
import School.Train.AppTrain (AppTrain, runTrainConduit)
import School.Train.GradientDescentPass (gradientDescentPass)
import School.Train.UpdateParams (UpdateParams)
import School.Train.StoppingCondition (StoppingCondition)
import School.Train.TrainState (TrainState)
import School.Unit.CostFunction (CostFunction)
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation)

stopping :: StoppingCondition a
         -> ConduitM ()
                     Void
                     (AppTrain a)
                     ()
stopping condition = do
  state <- get
  if condition state
    then return ()
    else do
      _ <- await
      return ()

gradientDescent :: ConduitM ()
                            (UnitActivation a)
                            (AppTrain a)
                            ()
                -> [Unit a]
                -> CostFunction a
                -> UpdateParams a
                -> StoppingCondition a
                -> TrainState a
                -> Either String
                          (TrainState a)
gradientDescent source
                units
                cost
                update
                condition
                initState = 
  let pass = gradientDescentPass units cost update
      iterations = source
                .| pass
                .| stopping condition
      result = runTrainConduit iterations $ initState
  in either Left
            (\(_, state) -> Right state)
            result
