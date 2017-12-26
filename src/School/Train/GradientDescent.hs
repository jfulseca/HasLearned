module School.Train.GradientDescent
( gradientDescent ) where

import Conduit ((.|), ConduitM, mapMC, runConduit, sinkNull, takeWhileC)
import Control.Monad.State.Lazy (get)
import Data.Maybe (isJust)
import Numeric.LinearAlgebra (Matrix)
import School.Train.AppTrain (AppTrain, runAppTrain)
import School.FileIO.MatrixSource (MatrixSource)
import School.Train.GradientDescentPass (gradientDescentPass)
import School.Train.IterationHandler (IterationHandler(..))
import School.Train.UpdateParams (UpdateParams)
import School.Train.StoppingCondition (StoppingCondition(..))
import School.Train.TrainState (TrainState)
import School.Types.Slinky (Slinky(..))
import School.Unit.CostFunction (CostFunction(..))
import School.Unit.Unit (Unit)
import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitForward (ForwardStack)
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

toStack :: ConduitM (Matrix a) (ForwardStack a) (AppTrain a) ()
toStack = mapMC $ \matrix ->
  return ([BatchActivation matrix], SNil)

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
  let pass = source
          .| toStack
          .| gradientDescentPass units cost update
          .| runHandler handler
          .| stopping condition
          .| takeWhileC isJust
          .| sinkNull
  result <- (runAppTrain initState) . runConduit $ pass
  return $ snd <$> result
