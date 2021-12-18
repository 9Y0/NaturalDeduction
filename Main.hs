module Main where

import Control.Applicative (empty, (<|>))
import Data.List (find, intersperse)
import GHC.Natural (Natural)
import Solver
  ( Solver (solve),
    getState,
    liftMaybe,
    modifyState,
  )

data Formula = Falsum | Atom Natural | And Formula Formula | Or Formula Formula | Impl Formula Formula
  deriving (Eq)

instance Show Formula where
  showsPrec _ Falsum = ("\\bot" ++)
  showsPrec _ (Atom n) = if n < 10 then ("A_" ++) . (show n ++) else ("A_{" ++) . (show n ++) . ("}" ++)
  showsPrec p (And f1 f2) = showParen (p > 3) $ showsPrec 3 f1 . (" \\land " ++) . showsPrec 3 f2
  showsPrec p (Or f1 f2) = showParen (p > 2) $ showsPrec 2 f1 . (" \\lor " ++) . showsPrec 2 f2
  showsPrec _ (Impl f1 Falsum) = ("\\lnot " ++) . showsPrec 5 f1
  showsPrec p (Impl f1 f2) = showParen (p >= 4) $ showsPrec 4 f1 . (" \\rightarrow " ++) . showsPrec 4 f2

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
proof f theory = fst <$> solve (proof' f theory []) 0

proof' :: Formula -> Theory -> [Assumption] -> Solver AssumptionCounter DeductionTree
proof' f theory assumptions =
  proofFromTheory f theory assumptions
    <|> proofFromAssumption f assumptions
    <|> proofByIntroduction f theory assumptions

-- TODO: Assumptions are now always rendered, even if they are not directly used
proofFromTheory :: Formula -> Theory -> [Assumption] -> Solver AssumptionCounter DeductionTree
proofFromTheory f theory assumptions = do
  if f `elem` theory
    then return $ Tree f Nothing $ map Assumption' assumptions
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
