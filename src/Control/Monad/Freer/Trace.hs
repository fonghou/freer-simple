{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
    , outputToTrace
    , traceToOutput
    , traceEffect
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Output
import Control.Monad.IO.Class

import qualified Debug.Trace as Debug

-- | A Trace effect; takes a 'String' and performs output.
data Trace a where
  Trace :: String -> Trace ()

-- | Printing a string in a trace.
trace :: Member Trace effs => String -> Eff effs ()
trace = send . Trace

{-# INLINE trace #-}

-- | An 'IO' handler for 'Trace' effects.
runTrace :: forall m effs.
         (LastMember m effs, MonadIO m)
         => Eff (Trace ': effs) ~> Eff effs
runTrace = subsume @m $ \(Trace s) -> liftIO $ Debug.traceIO s

{-# INLINE runTrace #-}

------------------------------------------------------------------------------
-- | Run a 'Trace' effect by ignoring all of its messages.
ignoreTrace :: Eff (Trace ': effs) ~> Eff effs
ignoreTrace = interpret $ \case Trace _ -> pure ()

{-# INLINE ignoreTrace #-}

------------------------------------------------------------------------------
-- | Get the result of a 'Trace' effect as a list of 'String's.
runTraceList :: Eff (Trace ': r) a -> Eff r ([String], a)
runTraceList = runOutputList . reinterpret (\case Trace m -> output m)

{-# INLINE runTraceList #-}

------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
traceToOutput :: Member (Output String) effs => Eff (Trace ': effs) ~> Eff effs
traceToOutput = interpret $ \case Trace m -> output m

{-# INLINE traceToOutput #-}

------------------------------------------------------------------------------
-- | Transform an 'Output' 'String' effect into a 'Trace' effect.
outputToTrace :: Member Trace effs
              => (o -> String) -> Eff (Output o ': effs) ~> Eff effs
outputToTrace show' = interpret $ \case Output o -> trace $ show' o

{-# INLINE outputToTrace #-}

traceEffect :: forall e r. (Members '[e, Trace] r)
            => (forall x. e x -> String) -> Eff r ~> Eff r
traceEffect show' (Eff m) = Eff $ \k -> m $ \u ->
  case prj @e u of
    Just e -> do
      runEff (trace $ show' e) k
      k u
    Nothing -> k u
