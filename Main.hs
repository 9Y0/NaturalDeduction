{-# LANGUAGE LambdaCase #-}

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
    constructFromKnownDeduction,
    find,
    findKnown,
    getAssumptions,
    getTheory,
    runSolver,
    withAssumption,
    withoutKnown,
  )
import Util (ifM)

conclusion :: DeductionTree -> Formula
conclusion (Tree f _ _) = f
conclusion (Assumption' (Assumption f _)) = f

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
proof' f = proofFromTheory f <|> proofFromAssumption f <|> proofByIntroduction f <|> proofByElimination f

-- TODO: Assumptions are now always rendered, even if they are not directly used
proofFromTheory :: Formula -> Solver DeductionTree
proofFromTheory f =
  ifM
    ((f `elem`) <$> getTheory)
    (Tree f Nothing . map Assumption' <$> getAssumptions)
    empty

proofFromAssumption :: Formula -> Solver DeductionTree
proofFromAssumption f = Tree f Nothing . return . Assumption' <$> find (\(Assumption f' _) -> f == f') getAssumptions

proofByIntroduction :: Formula -> Solver DeductionTree
proofByIntroduction f = case f of
  Falsum -> empty
  Atom _ -> empty
  And lhs rhs -> Tree f Nothing <$> sequence [proof' lhs, proof' rhs]
  Or lhs rhs -> Tree f Nothing . return <$> (proof' lhs <|> proof' rhs)
  Impl lhs rhs -> do
    (rhsProof, assumptionNumber) <- proof' rhs `withAssumption` Left lhs
    return $ Tree f (Just assumptionNumber) [rhsProof]

proofByElimination :: Formula -> Solver DeductionTree
proofByElimination f = eliminateAnd <|> eliminateImplication <|> eliminateOr <|> eliminateFalsum
  where
    eliminateAnd :: Solver DeductionTree
    eliminateAnd = Tree f Nothing . return <$> findKnown (\case And lhs rhs -> f == lhs || f == rhs; _ -> False)

    eliminateImplication :: Solver DeductionTree
    eliminateImplication = do
      constructFromKnownDeduction
        ( \deduction -> case conclusion deduction of
            Impl lhs rhs ->
              if rhs /= f
                then empty
                else (\lhsDeduction -> Tree f Nothing [lhsDeduction, deduction]) <$> proof' lhs
            _ -> empty
        )

    eliminateOr :: Solver DeductionTree
    eliminateOr =
      constructFromKnownDeduction
        ( \deduction -> case conclusion deduction of
            Or lhs rhs ->
              ( do
                  (leftDeduction, assumptionNumber) <- proof' f `withAssumption` Left lhs
                  (rightDeduction, _) <- proof' f `withAssumption` Right (Assumption rhs assumptionNumber)
                  return $ Tree f (Just assumptionNumber) [deduction, leftDeduction, rightDeduction]
              )
                `withoutKnown` deduction
            _ -> empty
        )

    eliminateFalsum :: Solver DeductionTree
    eliminateFalsum =
      constructFromKnownDeduction
        ( \deduction -> case conclusion deduction of
            Falsum -> return $ Tree f Nothing [deduction]
            _ -> empty
        )

main :: IO ()
main = undefined
