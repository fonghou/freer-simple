module Monad where

import Control.Monad (ap, join, liftM)
import Prelude

-----------------------------------------------------------------------------
-- https://apfelmus.nfshost.com/articles/operational-monad.html

data Program stmt a where
  Then :: stmt a -> (a -> Program stmt b) -> Program stmt b
  Return :: a -> Program stmt a

deriving instance Functor (Program stmt)

instance Functor stmt => Monad (Program stmt) where
  return = Return
  (Return a) >>= xs    = xs a
  (s `Then` xs) >>= ys = s `Then` (\a -> xs a >>= ys)

data State s a where
  Get :: State s s
  Put :: s -> State s ()

runState :: s -> Program (State s) a -> (s, a)
runState s (Return x)       = (s, x)
runState s (Get `Then` k)   = runState s (k s)
runState _ (Put s `Then` k) = runState s (k ())

-----------------------------------------------------------------------------
-- Free Monad

data Free f a
  = Pure a
  | Join (f (Free f a))
  deriving (Functor)

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Join m >>= f = Join (fmap (>>= f) m)

-----------------------------------------------------------------------------
-- Church Free

newtype F f a = F
  {runF :: forall r. (f r -> r) -> (a -> r) -> r}
  deriving (Functor)

instance Monad (F f) where
  return a = F $ \_ k -> k a
  m >>= f = F $ \alg k -> runF m alg $ \a -> runF (f a) alg k

toFree :: F f a -> Free f a
toFree (F f) = f Join Pure

foldF :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldF alg (F f) = f (join . alg) pure

-----------------------------------------------------------------------------
-- Freer Monad

newtype Eff f a = Eff
  {runEff :: forall m. Monad m => (forall x. f x -> m x) -> m a}

instance Monad (Eff f) where
  return a = Eff $ \_ -> return a
  m >>= f = Eff $ \alg -> runEff m alg >>= (\a -> runEff (f a) alg)

-----------------------------------------------------------------------------
-- https://blog.poisson.chat/posts/2019-10-26-reasonable-continuations.html

newtype Cont r a = Cont {(>>-) :: (a -> r) -> r}
  deriving (Functor)

instance Monad (Cont r) where
  return a = Cont $ \k -> k a
  m >>= f = Cont $ \k -> m >>- (\a -> f a >>- k)

runCont :: Cont r r -> r
runCont m = m >>- id

reset :: Cont a a -> Cont r a
reset = return . runCont

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont (runCont . f)

type Pure a = forall r. Cont r a

runPure :: Pure a -> a
runPure (Cont m) = m id

type Except e a = forall r. Cont (Either e r) a

runExcept :: Except e a -> Either e a
runExcept (Cont m) = m Right

throw :: e -> Except e a
throw e = Cont (\_ -> Left e)

type State' s a = forall r. Cont (s -> r) a

runState' :: Cont (s -> (a, s)) a -> s -> (a, s)
runState' (Cont m) = m (,)

get :: Cont (s -> s) s
get = Cont (\k s -> k s s)

put :: s -> Cont (s -> s) ()
put s = Cont (\k s -> k () s)

type ContT r m a = Cont (m r) a

lift :: Monad m => m a -> ContT r m a
lift m = Cont (m >>=)

type Codensity m a = forall r. Cont (m r) a

lowerCodensity :: Applicative m => Codensity m a -> m a
lowerCodensity m = m >>- pure

improve :: Functor f => Codensity (Free f) a -> Free f a
improve = lowerCodensity

-------------------------------------------------------------------------------

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Applicative (Program f) where
  pure = return
  (<*>) = ap

instance Applicative (F f) where
  pure = return
  (<*>) = ap

instance Functor (Eff f) where
  fmap = liftM

instance Applicative (Eff f) where
  pure = return
  (<*>) = ap

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap
