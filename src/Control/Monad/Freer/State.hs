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
  , modify'
  , gets

    -- * State Handlers
  , runState
  , evalState
  , execState

    -- * State Utilities
  , transactState
  , transactState'
  ) where

import Data.Proxy (Proxy)

import Control.Monad.Freer (Eff, Member, send, type (~>))
import Control.Monad.Freer.Interpretation
import qualified Control.Monad.Trans.State.Strict as S

-- | Strict 'State' effects: one can either 'Get' values or 'Put' them.
data State s r where
  Get :: State s s
  Put :: !s -> State s ()

-- | Retrieve the current value of the state of type @s :: *@.
get :: forall s effs. Member (State s) effs => Eff effs s
get = send Get
{-# INLINE get #-}

-- | Set the current state to a specified value of type @s :: *@.
put :: forall s effs. Member (State s) effs => s -> Eff effs ()
put s = send (Put s)
{-# INLINE put #-}

-- | Modify the current state of type @s :: *@ using provided function
-- @(s -> s)@.
modify :: forall s effs. Member (State s) effs => (s -> s) -> Eff effs ()
modify f = do
  s <- get
  put $ f s
{-# INLINE modify #-}

------------------------------------------------------------------------------
-- | A variant of 'modify' in which the computation is strict in the
-- new state.
modify' :: forall s effs. Member (State s) effs => (s -> s) -> Eff effs ()
modify' f = do
  s <- get
  put $! f s
{-# INLINABLE modify' #-}

-- | Retrieve a specific component of the current state using the provided
-- projection function.
gets :: forall s a effs. Member (State s) effs => (s -> a) -> Eff effs a
gets f = f <$> get
{-# INLINE gets #-}

-- | Handler for 'State' effects.
-- NB: returns (s, a), swapped from MTL State
runState :: forall s effs a. s -> Eff (State s ': effs) a -> Eff effs (s, a)
runState = stateful stateNat
{-# INLINE runState #-}

stateNat :: State s ~> S.StateT s (Eff r)
stateNat = \case
  Get   -> S.get
  Put s -> S.put s
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
  :: forall s effs a
   . Member (State s) effs
  => Eff effs a
  -> Eff effs a
transactState m = do
    s0 <- get @s
    (s, x) <- interposeState stateNat s0 m
    put s
    pure x

-- | Like 'transactState', but @s@ is specified by providing a 'Proxy'
-- instead of requiring @TypeApplications@.
transactState'
  :: forall s effs a
   . Member (State s) effs
  => Proxy s
  -> Eff effs a
  -> Eff effs a
transactState' _ = transactState @s
{-# INLINE transactState' #-}
