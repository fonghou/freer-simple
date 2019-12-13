{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module:       Control.Monad.Freer.State
-- Description:  State effects, for state-carrying computations.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'State' effects. Handy for passing an updatable state
-- through a computation.
--
-- Some computations may not require the full power of 'State' effect:
--
-- * For a read-only state, see "Control.Monad.Freer.Reader".
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Freer.Writer".
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.State
    ( -- * State Effect
      State(..)
      -- * State Operations
    , get
    , put
    , modify
    , gets
      -- * State Handlers
    , evalState
    , execState
    , stateToST
    , runState
    , runStateSTRef
    , runAtomicStateIORef
    , runAtomicStateTVar
      -- * State Utilities
    , transactState
    , transactState'
    ) where

import Control.Concurrent.STM
import Control.Monad.Freer
import Control.Monad.Freer.Interpretation
import Control.Monad.IO.Class
import Control.Monad.ST
import qualified Control.Monad.Trans.State.Strict as S

import Data.IORef
import Data.Proxy ( Proxy )
import Data.STRef
import Data.Tuple ( swap )

-- | Strict 'State' effects: one can either 'Get' values or 'Put' them.
data State s r where
  Get :: State s s
  Modify :: (s -> s) -> State s ()

-- | Retrieve the current value of the state of type @s :: *@.
get :: forall s effs. Member (State s) effs => Eff effs s
get = send Get

{-# INLINE get #-}

-- | Set the current state to a specified value of type @s :: *@.
put :: forall s effs. Member (State s) effs => s -> Eff effs ()
put s = modify (const s)

{-# INLINE put #-}

-- | Modify the current state of type @s :: *@ using provided function
-- @(s -> s)@.
modify :: forall s effs. Member (State s) effs => (s -> s) -> Eff effs ()
modify = send . Modify

{-# INLINE modify #-}

-- | Retrieve a specific component of the current state using the provided
-- projection function.
gets :: forall s a effs. Member (State s) effs => (s -> a) -> Eff effs a
gets f = f <$> get

{-# INLINE gets #-}

-- | A pure handler for 'State' effects.
-- NB: State tuple (s, a) is swapped from MTL State (a, s)
runState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs (s, a)
runState = stateful stateNat

{-# INLINE [3] runState #-}

stateNat :: State s ~> S.StateT s (Eff r)
stateNat = \case Get      -> S.get
                 Modify f -> S.modify' f

{-# INLINE stateNat #-}

-- | Run a 'State' effect, returning only the final state.
execState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs s
execState s = fmap fst . runState s

{-# INLINE execState #-}

-- | Run a State effect, discarding the final state.
evalState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs a
evalState s = fmap snd . runState s

{-# INLINE evalState #-}

-- | An encapsulated State handler, for transactional semantics. The global
-- state is updated only if the 'transactState' finished successfully.
--
-- GHC cannot infer the @s@ type parameter for this function, so it must be
-- specified explicitly with @TypeApplications@. Alternatively, it can be
-- specified by supplying a 'Proxy' to 'transactState''.
transactState
  :: forall s effs a. Member (State s) effs => Eff effs a -> Eff effs a
transactState m = do
  s0 <- get @s
  (s, x) <- interposeState stateNat s0 m
  put s
  pure x

{-# INLINE transactState #-}

-- | Like 'transactState', but @s@ is specified by providing a 'Proxy'
-- instead of requiring @TypeApplications@.
transactState' :: forall s effs a.
               Member (State s) effs
               => Proxy s
               -> Eff effs a
               -> Eff effs a
transactState' _ = transactState @s

{-# INLINE transactState' #-}

-- | Run 'State' effects by transforming it into 'ST' state.
stateToST :: forall s st effs a.
          LastMember (ST st) effs
          => s
          -> Eff (State s ': effs) a
          -> Eff effs (s, a)
stateToST s eff = do
  ref <- sendM $ newSTRef s
  res <- runStateSTRef ref eff
  state <- sendM $ readSTRef ref
  return (state, res)

{-# INLINE stateToST #-}

-- | Run 'State' effects in terms of operations in 'ST'.
runStateSTRef :: forall s st effs a.
              LastMember (ST st) effs
              => STRef st s
              -> Eff (State s ': effs) a
              -> Eff effs a
runStateSTRef ref = subsume @(ST st) $ \case
  Get      -> readSTRef ref
  Modify f -> modifySTRef' ref f

{-# INLINE runStateSTRef #-}

-- | An atomic IORef handler for 'State' effects.
runAtomicStateIORef :: forall s m effs a.
                    (LastMember m effs, MonadIO m)
                    => IORef s
                    -> Eff (State s ': effs) a
                    -> Eff effs a
runAtomicStateIORef ref = subsume @m $ \case
  Get      -> liftIO $ readIORef ref
  Modify f -> liftIO $ atomicModifyIORef' ref (\s -> (f s, ()))

{-# INLINE runAtomicStateIORef #-}

-- | An atomic TVar handler for 'State' effects.
runAtomicStateTVar :: forall s m effs a.
                   (LastMember m effs, MonadIO m)
                   => TVar s
                   -> Eff (State s ': effs) a
                   -> Eff effs a
runAtomicStateTVar tvar = subsume @m $ \case
  Get      -> liftIO $ readTVarIO tvar
  Modify f -> liftIO $ atomically $ modifyTVar' tvar f

{-# INLINE runAtomicStateTVar #-}

----------------------------------------------------------------------------
{-# RULES "runState/reinterpret"
    forall s e (f :: forall x. e x -> Eff (State s ': r) x).
      runState s (reinterpret f e) =
        stateful (\x -> S.StateT (\s' -> fmap swap $ runState s' $ f x)) s e
  #-}
