module Monad where

import Control.Monad (join)

data List a   = Nil    | Cons a (List a)
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)

foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free as) = f as >>= foldFree f

newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }

newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (forall x. f x -> m x) -> m a }

instance Monad (F f) where
  F m >>= f    = F      $ \cont kf -> m (\a -> runF (f a) cont kf) kf
  return a     = F (\cont _ -> cont a)

instance Monad (Freer f) where
  Freer m >>= f = Freer $ \cont -> m cont >>= (\a -> runFreer (f a) cont)
  return a      = Freer (\_ -> return a)

foldF :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldF f (F m) = m return (join . f)

-- interpret :: (forall x. eff x -> Eff r x) -> Eff (eff ': r) a -> Eff r a
-- interpret f (Freer m) = Freer $ \k -> m $ \u ->
--   case decomp u of
--     Left x -> k x
--     Right y -> runFreer (f y) k
--

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
  Cont m >>= f = Cont (\cont -> m (\a -> runCont (f a) cont))
  return a     = Cont (\cont -> cont a)

type Id a = forall r. Cont r a
runId (Cont m) = m id

type Except e a = forall r. Cont (Either e r) a
runExcept (Cont m) = m Right
throw  e = Cont (\ _k -> Left e)

type State s a = forall r. Cont (s -> r) a
runState (Cont m) = m (,)
get = Cont (\k s -> k s s)
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
