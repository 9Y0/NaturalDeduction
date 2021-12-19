{-# LANGUAGE InstanceSigs #-}

module State where

import Control.Applicative (Alternative, empty, (<|>))
import qualified Data.Bifunctor
import Control.Monad.Trans.Class (MonadTrans(lift))

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f state = StateT $ fmap (Data.Bifunctor.first f) . runStateT state

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT solverF <*> StateT solverX = StateT $ \s -> do
    (f, s') <- solverF s
    (x, s'') <- solverX s'
    return (f x, s'')

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty :: StateT s m a
  empty = StateT $ const empty

  (<|>) :: StateT s m a -> StateT s m a -> StateT s m a
  stateLeft <|> stateRight = StateT $ \s -> runStateT stateLeft s <|> runStateT stateRight s

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  state >>= f = StateT $ \s -> do
    (x, s') <- runStateT state s
    runStateT (f x) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift monad = StateT $ \s -> do
    x <- monad
    return (x, s)

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return ((), f s)
