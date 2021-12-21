module Solver where

import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.List as List (find)
import Formula (Assumption (Assumption), AssumptionCounter, Formula, Theory)

newtype SolverEnviron = Environ {theory :: Theory}

data SolverState = State {counter :: AssumptionCounter, assumptions :: [Assumption]}

type Solver = ReaderT SolverEnviron (StateT SolverState Maybe)

initialState :: SolverState
initialState = State 0 []

runSolver :: Solver a -> SolverEnviron -> Maybe (a, SolverState)
runSolver s environ = runStateT (runReaderT s environ) initialState

getTheory :: Solver Theory
getTheory = asks theory

getAssumptions :: Solver [Assumption]
getAssumptions = gets assumptions

getAssumptionCounter :: Solver AssumptionCounter
getAssumptionCounter = gets counter

withAssumption :: Solver a -> Formula -> Solver (a, AssumptionCounter)
withAssumption solver assumption = do
  assumptionNumber <- getAssumptionCounter
  addAssumption assumption assumptionNumber
  incrementAssumptionCounter
  result <- solver
  popAssumption
  return (result, assumptionNumber)

incrementAssumptionCounter :: Solver ()
incrementAssumptionCounter = modify (\state -> state {counter = counter state + 1})

addAssumption :: Formula -> AssumptionCounter -> Solver ()
addAssumption assumption number = modify (\state -> state {assumptions = Assumption assumption number : assumptions state})

popAssumption :: Solver ()
popAssumption = modify (\state -> state {assumptions = case assumptions state of [] -> []; (_:other) -> other})

find :: (a -> Bool) -> Solver [a] -> Solver a
find predicate solver = solver >>= lift . lift . List.find predicate
