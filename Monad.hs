module Monad where

import Control.Monad (join)

data List a   = Nil    | Cons a (List a)
data Free f a = Pure a | Join (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Join m >>= f = Join (fmap (>>= f) m)
                    -- slow!

foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree alg (Join as) = alg as >>= foldFree alg

-----------------------------------------------------------------------------
-- ReaderT (f r -> r) (Cont r) a
newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }

foldF :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldF alg (F m) = m return (join . alg)

-- runFreer = flip foldF
newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (forall x. f x -> m x) -> m a }

-- ReaderT alg m where
-- alg ~ (forall x. f x -> m x)
-- alg = interpret g . interpret f
instance Monad (Freer f) where
  return a      = Freer (\_ -> return a)
  Freer m >>= f = Freer $ \alg -> m alg >>= (\a -> runFreer (f a) alg)
                       -- (foldF alg m) >>= (\a -> foldF alg (f a))

instance Monad (F f) where
  return a     = F (\_return _ -> _return a)
  F m >>= f    = F $ \_return alg -> m (\a -> runF (f a) _return alg) alg

-----------------------------------------------------------------------------
newtype Cont r a = Cont { (>>-) :: (a -> r) -> r }

instance Monad (Cont r) where
  return a     = Cont (\_return -> _return a)
  Cont m >>= f = Cont $ \_return -> m $ \a -> f a >>- _return

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
throw e = Cont (\ _k -> Left e)

type State s a = forall r. Cont (s -> r) a
runState :: Cont (b -> (a, b)) a -> b -> (a, b)
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
-- (>>=) :: Monad m => m a -> (a -> m r) -> m r
lift :: Monad m => m a -> ContT r m a
lift m  = Cont (m >>=)

type Codensity m a = forall r. Cont (m r) a
type F' f a = Codensity (Free f) a

-------------------------------------------------------------------------------
instance Functor (Cont r) where
  fmap f ma = pure f <*> ma

instance Applicative (Cont r) where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Functor (F f) where
  fmap f a = pure f <*> a

instance Applicative (Freer f) where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Functor (Freer f) where
  fmap f a = pure f <*> a

instance Applicative (F f) where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }
instance Functor f => Functor (Free f)
 where fmap f ma = pure f <*> ma

instance Functor f => Applicative (Free f) where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }
