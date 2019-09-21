{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Freer.Interpretation where

import Data.Tuple (swap)

import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S
import           Control.Monad.Freer.Internal


------------------------------------------------------------------------------
-- | Interpret an effect in terms of another effect in the stack.
subsume
  :: Member eff2 r
  => (eff1 ~> eff2)
  -> Eff (eff1 ': r) ~> Eff r
subsume = naturally id
{-# INLINE subsume #-}


------------------------------------------------------------------------------
-- | Interpret an effect as a monadic action in 'Eff r'.
interpret :: (eff ~> Eff r) -> Eff (eff ': r) ~> Eff r
interpret f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x -> k x
    Right y -> runFreer (f y) k
{-# INLINE[3] interpret #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to intermediate state.
stateful
  :: (eff ~> S.StateT s (Eff r))
  -> s
  -> Eff (eff ': r) a -> Eff r (s, a)
stateful f s (Freer m) = fmap swap $ Freer $ \k ->
  flip S.runStateT s $ m $ \u ->
    case decomp u of
      Left  x -> lift $ k x
      Right y -> hoist (usingFreer k) $ f y
{-# INLINE stateful #-}
-- NB: @stateful f s = transform (flip S.runStateT s) f@, but is not
-- implemented as such, since 'transform' is available only >= 8.6.0


------------------------------------------------------------------------------
-- | flip stateful
withStateful
  :: s
  -> (eff ~> S.StateT s (Eff r))
  -> Eff (eff ': r) a -> Eff r (s, a)
withStateful s f = stateful f s
{-# INLINE withStateful #-}


------------------------------------------------------------------------------
-- | Replace the topmost layer of the effect stack with another. This is often
-- useful for interpreters which would like to introduce some intermediate
-- effects before immediately handling them.
reinterpret
  :: forall f g r . (f ~> Eff (g ': r)) -> Eff (f : r) ~> Eff (g : r)
reinterpret f  = interpret f . raiseUnder
{-# INLINE reinterpret #-}

reinterpret2
  :: forall f g1 g2 r 
  . (f ~> Eff (g1 ': g2 ': r))
  -> Eff (f : r) ~> Eff (g1 : g2 : r)
reinterpret2 f  = interpret f . raiseUnder2
{-# INLINE reinterpret2 #-}

reinterpret3
  :: forall f g1 g2 g3 r 
  . (f ~> Eff (g1 ': g2 ': g3 ': r)) 
  -> Eff (f : r) ~> Eff (g1 ': g2 ': g3 ': r)
reinterpret3 f = interpret f . raiseUnder3
{-# INLINE reinterpret3 #-}

reinterpret4
  :: forall f g1 g2 g3 g4 r 
  . (f ~> Eff (g1 ': g2 ': g3 ': g4 ': r))
  -> Eff (f : r) ~> Eff (g1 ': g2 ': g3 ': g4 ': r)
reinterpret4 f = interpret f . raiseUnder4
{-# INLINE reinterpret4 #-}


#if __GLASGOW_HASKELL__ >= 806
------------------------------------------------------------------------------
-- | Run an effect via the side-effects of a monad transformer.
transform
  :: ( MonadTrans t
     , forall m. Monad m => Monad (t m)
     )
  => (forall m. Eff r ~> m -> t (Eff r) ~> t m)
     -- ^ The strategy for hoisting a natural transformation. This is usually
     -- just 'hoist'.
  -> (forall m. t m a -> m b)
     -- ^ The strategy for getting out of the monad transformer. This is
     -- usually just @runWhateverT@.
  -> (eff ~> t (Eff r))
  -> Eff (eff ': r) a
  -> Eff r b
transform hoist' lower f (Freer m) =
  Freer $ \k -> lower $ m $ \u ->
    case decomp u of
      Left  x -> lift $ k x
      Right y -> hoist' (usingFreer k) $ f y
{-# INLINE[3] transform #-}
#endif


------------------------------------------------------------------------------
-- | Run an effect, potentially short circuiting in its evaluation.
shortCircuit
  :: (eff ~> E.ExceptT e (Eff r))
  -> Eff (eff ': r) a
  -> Eff r (Either e a)
shortCircuit f (Freer m) = Freer $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left  x -> lift $ k x
    Right y -> hoist (usingFreer k) $ f y
{-# INLINE shortCircuit #-}
-- NB: @shortCircuit = transform E.runExceptT@, but is not implemented as such,
-- since 'transform' is available only >= 8.6.0


------------------------------------------------------------------------------
-- |Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
interpose
  :: Member eff r
  => (eff ~> Eff r)
  -> Eff r ~> Eff r
interpose f (Freer m) = Freer $ \k -> m $ \u ->
  case prj u of
    Nothing -> k u
    Just e  -> usingFreer k $ f e
{-# INLINE interpose #-}


------------------------------------------------------------------------------
-- | Like 'Interpose', but with access to intermediate state.
interposeState
  :: Member eff r
  => (eff ~> S.StateT s (Eff r))
  -> s
  -> Eff r a -> Eff r (s, a)
interposeState f s (Freer m) = fmap swap $ Freer $ \k ->
  usingFreer k $ flip S.runStateT s $ m $ \u ->
    case prj u of
      Nothing -> lift $ liftEff u
      Just e  -> f e
{-# INLINE interposeState #-}


------------------------------------------------------------------------------
-- | Run an effect with an explicit continuation to the final result. If you're
-- not sure why you might need this, you probably don't.
--
-- Note that this method is slow---roughly 10x slower than the other combinators
-- available here. If you just need short circuiting, consider using
-- 'shortCircuit' instead.
relay
  :: (a -> Eff r b)
  -> (forall x. eff x -> (x -> Eff r b) -> Eff r b)
  -> Eff (eff ': r) a
  -> Eff r b
relay pure' bind' (Freer m) = Freer $ \k ->
  usingFreer k $ flip runContT pure' $ m $ \u ->
    case decomp u of
      Left  x -> lift $ liftEff x
      Right y -> ContT $ bind' y
{-# INLINE relay #-}


------------------------------------------------------------------------------
-- | Like 'interpose' and 'relay'.
interposeRelay
  :: Member eff r
  => (a -> Eff r b)
  -> (forall x. eff x -> (x -> Eff r b) -> Eff r b)
  -> Eff r a
  -> Eff r b
interposeRelay pure' bind' (Freer m) = Freer $ \k ->
  usingFreer k $ flip runContT pure' $ m $ \u ->
    case prj u of
      Nothing -> lift $ liftEff u
      Just y  -> ContT $ bind' y
{-# INLINE interposeRelay #-}


------------------------------------------------------------------------------
-- | Run an effect, potentially changing the entire effect stack underneath it.
naturally
  :: Member eff' r'
  => (Union r ~> Union r')
  -> (eff ~> eff')
  -> Eff (eff ': r) ~> Eff r'
naturally z f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x  -> k $ z x
    Right y -> k . inj $ f y
{-# INLINE naturally #-}
