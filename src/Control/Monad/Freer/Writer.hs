{-# LANGUAGE TupleSections #-}

-- |
-- Module:       Control.Monad.Freer.Writer
-- Description:  Composable Writer effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- 'Writer' effects, for writing\/appending values (line count, list of
-- messages, etc.) to an output. Current value of 'Writer' effect output is not
-- accessible to the computation.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Writer
    ( Writer(..)
    , tell
    , runWriter
    , execWriter
    , writerToOutput
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Interpretation
import Control.Monad.Freer.Output
import Control.Monad.Trans.State.Strict ( modify' )

import Data.Monoid ()

-- | Writer effects - send outputs to an effect environment.
data Writer w r where
  Tell :: w -> Writer w ()

-- | Send a change to the attached environment.
tell :: forall w effs. Member (Writer w) effs => w -> Eff effs ()
tell w = send (Tell w)
{-# INLINE tell #-}

-- | Simple handler for 'Writer' effects.
-- NB: Writer tuple (w, a) is swapped from MTL (a, w)
runWriter
  :: forall w effs a. Monoid w => Eff (Writer w ': effs) a -> Eff effs (w, a)
runWriter = stateful (\(Tell w) -> modify' (<> w)) mempty
{-# INLINE runWriter #-}

-- | Extract the output from a writer computation.
execWriter
  :: forall w effs a. Monoid w => Eff (Writer w ': effs) a -> Eff effs w
execWriter = fmap fst . runWriter
{-# INLINE execWriter #-}

-- | Transform a 'Writer' effect into a 'Output' effect.
writerToOutput :: Member (Output o) effs => Eff (Writer o ': effs) ~> Eff effs
writerToOutput = interpret $ \case Tell m -> output m
{-# INLINE writerToOutput #-}
