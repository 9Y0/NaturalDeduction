{-# LANGUAGE InstanceSigs #-}

module Solver where

import Control.Applicative (Alternative, empty, (<|>))
import qualified Data.Bifunctor

newtype Solver s a = Solver {solve :: s -> Maybe (a, s)}

instance Functor (Solver s) where
  fmap :: (a -> b) -> Solver s a -> Solver s b
  fmap f (Solver solver) = Solver $ fmap (Data.Bifunctor.first f) . solver

instance Applicative (Solver s) where
  pure :: a -> Solver s a
  pure x = Solver $ \s -> pure (x, s)

  (<*>) :: Solver s (a -> b) -> Solver s a -> Solver s b
  Solver solverF <*> Solver solverX = Solver $ \s -> case solverF s of
    Nothing -> Nothing
    Just (f, s') -> case solverX s' of
      Nothing -> Nothing
      Just (x, s'') -> Just (f x, s'')

instance Alternative (Solver s) where
  empty :: Solver s a
  empty = Solver $ const empty

  (<|>) :: Solver s a -> Solver s a -> Solver s a
  Solver solver1 <|> Solver solver2 = Solver $ \s -> solver1 s <|> solver2 s

instance Monad (Solver s) where
  (>>=) :: Solver s a -> (a -> Solver s b) -> Solver s b
  Solver solverX >>= f = Solver $ \s -> case solverX s of
    Nothing -> Nothing
    Just (x, s') -> solve (f x) s'

liftMaybe :: Maybe a -> Solver s a
liftMaybe Nothing = Solver $ const empty
liftMaybe (Just x) = Solver $ \s -> return (x, s)

getState :: Solver s s
getState = Solver $ \s -> Just (s, s)

modifyState :: (s -> s) -> Solver s ()
modifyState f = Solver $ \s -> Just ((), f s)
