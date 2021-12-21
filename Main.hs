module Main where

import Control.Applicative (empty, (<|>))
import Data.List (intersperse)
import Formula
  ( Assumption (Assumption),
    DeductionTree (Assumption', Tree),
    Formula (And, Atom, Falsum, Impl, Or),
    Theory,
  )
import Solver
  ( Solver,
    SolverEnviron (Environ),
    find,
    getAssumptions,
    getTheory,
    runSolver,
    withAssumption,
  )

printDeductionTree :: DeductionTree -> IO ()
printDeductionTree = go 0
  where
    spaces :: Int -> String
    spaces = flip replicate ' ' . (* 4)

    go :: Int -> DeductionTree -> IO ()
    go indentLevel (Assumption' (Assumption f counter)) = do
      putStr $ spaces indentLevel
      putChar '['
      putStr $ show f
      putStr "]^"
      print counter
    go indentLevel (Tree f _ []) = do
      putStr $ spaces indentLevel
      print f
    go indentLevel (Tree f maybeCounter inferences) = do
      putStr $ spaces indentLevel
      putStr "\\infer"
      case maybeCounter of
        Nothing -> putChar '\n'
        Just counter -> do putStr "[^"; putStr $ show counter; putStr "]\n"
      putStr $ spaces (indentLevel + 1)
      putChar '{'
      putStr $ show f
      putStr "}\n"
      putStr $ spaces (indentLevel + 1)
      putStr "{\n"
      sequence_ $ intersperse (putStr (spaces (indentLevel + 2)) >> putStrLn "&") $ map (go (indentLevel + 2)) inferences
      putStr $ spaces (indentLevel + 1)
      putStr "}\n"

proof :: Formula -> Theory -> Maybe DeductionTree
proof f theory = fst <$> runSolver (proof' f) (Environ theory)

proof' :: Formula -> Solver DeductionTree
proof' f = proofFromTheory f <|> proofFromAssumption f <|> proofByIntroduction f

-- TODO: Assumptions are now always rendered, even if they are not directly used
proofFromTheory :: Formula -> Solver DeductionTree
proofFromTheory f = do
  theory <- getTheory
  if f `elem` theory
    then Tree f Nothing . map Assumption' <$> getAssumptions
    else empty

proofFromAssumption :: Formula -> Solver DeductionTree
proofFromAssumption f = Tree f Nothing . return . Assumption' <$> find (\(Assumption f' _) -> f == f') getAssumptions

proofByIntroduction :: Formula -> Solver DeductionTree
proofByIntroduction f = case f of
  Falsum -> empty
  Atom _ -> empty
  And f1 f2 -> Tree f Nothing <$> sequence [proof' f1, proof' f2]
  Or f1 f2 -> Tree f Nothing . return <$> proof' f1 <|> proof' f2
  Impl f1 f2 -> do
    (p, assumptionNumber) <- proof' f2 `withAssumption` f1
    return $ Tree f (Just assumptionNumber) [p]

main :: IO ()
main = undefined
