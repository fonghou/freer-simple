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
  , ignoreTrace
  ) where

import Control.Monad.Freer.Internal (Eff, Member, send, type (~>))
import Control.Monad.Freer.Interpretation

-- | A Trace effect; takes a 'String' and performs output.
data Trace a where
  Trace :: String -> Trace ()

-- | Printing a string in a trace.
trace :: Member Trace effs => String -> Eff effs ()
trace = send . Trace

-- | An 'IO' handler for 'Trace' effects.
runTrace :: Member IO r => Eff (Trace ': r) ~> Eff r
runTrace = subsume @IO $ \(Trace s) -> putStrLn s

-- | Get the result of a 'Trace' effect as a list of 'String's.
-- runTraceList :: Eff (Trace ': r) a -> Eff r (a, [String])
-- runTraceList = runWriter . reinterpret
--   (\case
--      Trace o -> tell o)
-- {-# INLINE runTraceList #-}

------------------------------------------------------------------------------
-- | Run a 'Trace' effect by ignoring all of its messages.
--
-- @since 1.0.0.0
ignoreTrace :: Eff (Trace ': r) ~> Eff r
ignoreTrace = interpret $ \case
  Trace _ -> pure ()
{-# INLINE ignoreTrace #-}
