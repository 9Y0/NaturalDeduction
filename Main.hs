module Main where

data Formula = Falsum | Basic Int | And Formula Formula | Or Formula Formula | Impl Formula Formula
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

newtype Assumption = Assumption Formula
  deriving (Show)

data DeductionTree = Tree Formula [DeductionTree] | Assumption' Assumption
  deriving (Show)

type Theory = [Formula]

proof :: Formula -> Theory -> [Assumption] -> Maybe DeductionTree
proof = undefined

main :: IO ()
main = undefined