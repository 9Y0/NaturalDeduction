module Solver where

import Control.Applicative ((<|>))
import Control.Monad (liftM2, msum, when)
import Control.Monad.Reader (ReaderT (runReaderT), asks, local)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Either (isLeft)
import qualified Data.List as List (find)
import Formula (Assumption (Assumption), AssumptionCounter, DeductionTree (Assumption', Tree), Formula, Theory)

newtype SolverEnviron = Environ {theory :: Theory}

data SolverState = State {counter :: AssumptionCounter, assumptions :: [Assumption]}

type Solver = ReaderT SolverEnviron (StateT SolverState Maybe)

initialState :: SolverState
initialState = State 1 []

runSolver :: Solver a -> SolverEnviron -> Maybe (a, SolverState)
runSolver s environ = runStateT (runReaderT s environ) initialState

getTheory :: Solver Theory
getTheory = asks theory

getAssumptions :: Solver [Assumption]
getAssumptions = gets assumptions

getAssumptionCounter :: Solver AssumptionCounter
getAssumptionCounter = gets counter

withAssumption :: Solver a -> Either Formula Assumption -> Solver (a, AssumptionCounter)
withAssumption solver assumption = do
  assumptionNumber <- getAssumptionCounter
  addAssumption $ either (`Assumption` assumptionNumber) id assumption
  when (isLeft assumption) incrementAssumptionCounter
  result <- solver
  popAssumption
  return $ case assumption of
    Left _ -> (result, assumptionNumber)
    Right (Assumption _ number) -> (result, number)

incrementAssumptionCounter :: Solver ()
incrementAssumptionCounter = modify (\state -> state {counter = counter state + 1})

addAssumption :: Assumption -> Solver ()
addAssumption assumption = modify (\state -> state {assumptions = assumption : assumptions state})

popAssumption :: Solver ()
popAssumption = modify (\state -> state {assumptions = case assumptions state of [] -> []; (_ : other) -> other})

findKnown :: (Formula -> Bool) -> Solver DeductionTree
findKnown predicate =
  ((\formula -> Tree formula [] []) <$> find predicate getTheory)
    <|> (Assumption' <$> find (\(Assumption f _) -> predicate f) getAssumptions)

constructFromKnownDeduction :: (DeductionTree -> Solver a) -> Solver a
constructFromKnownDeduction p = known >>= msum . map p
  where
    known :: Solver [DeductionTree]
    known =
      liftM2
        (++)
        (map (\formula -> Tree formula [] []) <$> getTheory)
        (map Assumption' <$> getAssumptions)

{-
If it's a tree, then f was in the theory so we temporarily remove it from the environment.
Otherwise we temporarily remove the assumption from the state.
-}
withoutKnown :: Solver a -> DeductionTree -> Solver a
withoutKnown solver (Tree f _ _) = local (\env -> env {theory = filter (/= f) (theory env)}) solver
withoutKnown solver (Assumption' (Assumption f _)) = do
  origininalAssumptions <- getAssumptions
  modify (\state -> state {assumptions = filter (\(Assumption f' _) -> f /= f') (assumptions state)})
  result <- solver
  modify (\state -> state {assumptions = origininalAssumptions})
  return result

find :: (a -> Bool) -> Solver [a] -> Solver a
find predicate solver = solver >>= lift . lift . List.find predicate
