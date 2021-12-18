module Main where

import Control.Applicative (empty, (<|>))
import Data.List (find)
import GHC.Natural (Natural)
import Solver
  ( Solver (solve),
    getState,
    liftMaybe,
    modifyState,
  )

data Formula = Falsum | Atom Natural | And Formula Formula | Or Formula Formula | Impl Formula Formula
  deriving (Show, Eq)

infixr 4 -->

(-->) :: Formula -> Formula -> Formula
(-->) = Impl

infixr 3 /\

(/\) :: Formula -> Formula -> Formula
(/\) = And

infixr 2 \/

(\/) :: Formula -> Formula -> Formula
(\/) = Or

type AssumptionCounter = Natural

data Assumption = Assumption Formula AssumptionCounter
  deriving (Show)

type Theory = [Formula]

data DeductionTree = Tree Formula (Maybe AssumptionCounter) [DeductionTree] | Assumption' Assumption
  deriving (Show)

proof :: Formula -> Theory -> Maybe DeductionTree
proof f theory = fst <$> solve (proof' f theory []) 0

proof' :: Formula -> Theory -> [Assumption] -> Solver AssumptionCounter DeductionTree
proof' f theory assumptions =
  proofFromTheory f theory
    <|> proofFromAssumption f assumptions
    <|> proofByIntroduction f theory assumptions

proofFromTheory :: Formula -> Theory -> Solver AssumptionCounter DeductionTree
proofFromTheory f theory = do
  if f `elem` theory
    then return $ Tree f Nothing []
    else empty

proofFromAssumption :: Formula -> [Assumption] -> Solver AssumptionCounter DeductionTree
proofFromAssumption f assumptions = do
  assumption <- liftMaybe $ find (\(Assumption f' _) -> f == f') assumptions
  return $ Tree f Nothing [Assumption' assumption]

proofByIntroduction :: Formula -> Theory -> [Assumption] -> Solver AssumptionCounter DeductionTree
proofByIntroduction f theory assumptions = case f of
  Falsum -> empty
  Atom _ -> empty
  And f1 f2 -> Tree f Nothing <$> sequence [proof' f1 theory assumptions, proof' f2 theory assumptions]
  Or f1 f2 -> Tree f Nothing . return <$> proof' f1 theory assumptions <|> proof' f2 theory assumptions
  Impl f1 f2 -> do
    assumptionCounter <- getState
    modifyState (+ 1)
    p <- proof' f2 theory (Assumption f1 assumptionCounter : assumptions)
    return $ Tree f (Just assumptionCounter) [p]

main :: IO ()
main = undefined
