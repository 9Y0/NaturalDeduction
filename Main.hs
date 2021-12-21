module Main where

import Control.Applicative (empty, (<|>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.List (find, intersperse)
import Formula
  ( Assumption (..),
    AssumptionCounter,
    DeductionTree (..),
    Formula (..),
    Theory,
  )
import GHC.Natural (Natural)
import State (StateT (runStateT), get, modify)

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

type Solver = StateT AssumptionCounter Maybe DeductionTree

proof :: Formula -> Theory -> Maybe DeductionTree
proof f theory = fst <$> runStateT (proof' f theory []) 0

proof' :: Formula -> Theory -> [Assumption] -> Solver
proof' f theory assumptions =
  proofFromTheory f theory assumptions
    <|> proofFromAssumption f assumptions
    <|> proofByIntroduction f theory assumptions

-- TODO: Assumptions are now always rendered, even if they are not directly used
proofFromTheory :: Formula -> Theory -> [Assumption] -> Solver
proofFromTheory f theory assumptions = do
  if f `elem` theory
    then return $ Tree f Nothing $ map Assumption' assumptions
    else empty

proofFromAssumption :: Formula -> [Assumption] -> Solver
proofFromAssumption f assumptions = do
  assumption <- lift $ find (\(Assumption f' _) -> f == f') assumptions
  return $ Tree f Nothing [Assumption' assumption]

proofByIntroduction :: Formula -> Theory -> [Assumption] -> Solver
proofByIntroduction f theory assumptions = case f of
  Falsum -> empty
  Atom _ -> empty
  And f1 f2 -> Tree f Nothing <$> sequence [proof' f1 theory assumptions, proof' f2 theory assumptions]
  Or f1 f2 -> Tree f Nothing . return <$> proof' f1 theory assumptions <|> proof' f2 theory assumptions
  Impl f1 f2 -> do
    assumptionCounter <- get
    modify (+ 1)
    p <- proof' f2 theory (Assumption f1 assumptionCounter : assumptions)
    return $ Tree f (Just assumptionCounter) [p]

main :: IO ()
main = undefined
