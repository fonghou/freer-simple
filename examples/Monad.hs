module Monad where

import Control.Monad (join)
import Prelude

-----------------------------------------------------------------------------
-- Algebraic Effect

data Free f a where
  Pure :: a -> Free f a
  Bind :: f a -> (a -> Free f b) -> Free f b

data State' s a where
  Get :: State' s s
  Put :: s -> State' s ()

runState' :: s -> Free (State' s) a -> (s, a)
runState' s (Pure x) = (s, x)
runState' s (Get `Bind` k) = runState' s (k s)
runState' _ (Put s `Bind` k) = runState' s (k ())

-----------------------------------------------------------------------------
-- Church Free

newtype F f a = F
  {runF :: forall r. (f r -> r) -> (a -> r) -> r}

foldF :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldF alg (F f) = f (join . alg) return

instance Monad (F f) where
  return a = F $ \_ _return -> _return a
  F m >>= f = F $ \alg _return -> m alg (\a -> runF (f a) alg _return)

-----------------------------------------------------------------------------
-- https://terrorjack.com/posts/2018-11-28-simple-freer.html

newtype Eff f a = Eff
  {runFreer :: forall m. Monad m => (forall x. f x -> m x) -> m a}

instance Monad (Eff f) where
  return a = Eff $ \_ -> return a
  m >>= f = Eff $ \alg -> runFreer m alg >>= (\a -> runFreer (f a) alg)

-----------------------------------------------------------------------------
-- https://blog.poisson.chat/posts/2019-10-26-reasonable-continuations.html

newtype Cont r a = Cont {(>>-) :: (a -> r) -> r}

instance Monad (Cont r) where
  return a = Cont $ \_return -> _return a
  Cont m >>= f = Cont $ \_return -> m (\a -> f a >>- _return)

runC :: Cont r r -> r
runC m = m >>- id

reset :: Cont a a -> Cont r a
reset = return . runC

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont (runC . f)

type Pure a = forall r. Cont r a

runPure :: Cont r r -> r
runPure (Cont m) = m id

type Except e a = forall r. Cont (Either e r) a

runExcept :: Cont (Either a b) b -> Either a b
runExcept (Cont m) = m Right

throw :: a -> Cont (Either a b) r
throw e = Cont (\_k -> Left e)

type State s a = forall r. Cont (s -> r) a

runState :: Cont (s -> (a, s)) a -> s -> (a, s)
runState (Cont m) = m (,)

get :: Cont (s -> s) s
get = Cont (\k s -> k s s)

put :: s -> Cont (s -> s) ()
put s = Cont (\k _s -> k () s)

type ListT m a = Cont (m ()) a

runListT :: (a -> r) -> Cont r a -> r
runListT k (Cont m) = m k

choose :: Applicative m => ListT m Bool
choose = Cont (\k -> k True *> k False)

empty :: Applicative m => ListT m a
empty = Cont (\_k -> pure ())

type ContT r m a = Cont (m r) a

lift :: Monad m => m a -> ContT r m a
lift m = Cont (m >>=)

type Codensity m a = forall r. Cont (m r) a

type F' f a = Codensity (Free f) a

-------------------------------------------------------------------------------
instance Functor (F f) where
  fmap f a = pure f <*> a

instance Applicative (F f) where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Applicative (Eff f) where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Functor (Eff f) where
  fmap f a = pure f <*> a

instance Functor (Cont r) where
  fmap f ma = pure f <*> ma

instance Applicative (Cont r) where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)
