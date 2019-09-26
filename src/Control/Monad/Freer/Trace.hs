-- |
-- Module:       Control.Monad.Freer.Trace
-- Description:  Composable Trace effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Trace' effects. Trace allows one to debug the
-- operation of sequences of effects by outputing to the console.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Trace
  ( Trace(..)
  , trace
  , runTrace
  , runTraceList
  , ignoreTrace
  , traceToOutput
  , outputToTrace
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Output
import System.IO

-- | A Trace effect; takes a 'String' and performs output.
data Trace a where
  Trace :: String -> Trace ()

-- | Printing a string in a trace.
trace :: Member Trace effs => String -> Eff effs ()
trace = send . Trace
{-# INLINE trace #-}

-- | An 'IO' handler for 'Trace' effects.
runTrace :: Member IO effs => Eff (Trace ': effs) ~> Eff effs
runTrace = subsume @IO $ \(Trace s) -> hPutStrLn stderr s
{-# INLINE runTrace #-}

------------------------------------------------------------------------------
-- | Run a 'Trace' effect by ignoring all of its messages.
ignoreTrace :: Eff (Trace ': effs) ~> Eff effs
ignoreTrace = interpret $ \case
  Trace _ -> pure ()
{-# INLINE ignoreTrace #-}

------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
traceToOutput
    :: Member (Output String) effs
    => Eff (Trace ': effs) ~> Eff effs
traceToOutput = interpret $ \case
  Trace m -> output m
{-# INLINE traceToOutput #-}

------------------------------------------------------------------------------
-- | Get the result of a 'Trace' effect as a list of 'String's.
runTraceList
    :: Eff (Trace ': r) a
    -> Eff r ([String], a)
runTraceList = runOutputList . reinterpret (
  \case
    Trace m -> output m
  )
{-# INLINE runTraceList #-}

------------------------------------------------------------------------------
-- | Transform an 'Output' 'String' effect into a 'Trace' effect.
outputToTrace
    :: (Show o , Member Trace effs)
    => Eff (Output o ': effs) ~> Eff effs
outputToTrace = interpret $ \case
  Output o -> trace $ show o
{-# INLINE outputToTrace #-}
