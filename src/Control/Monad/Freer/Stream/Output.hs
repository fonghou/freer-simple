module Control.Monad.Freer.Stream.Output
    ( module Control.Monad.Freer.Output
    , runOutputStream
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Output
import Control.Monad.Freer.State

import Data.Functor.Of

import qualified Streaming as S
import qualified Streaming.Prelude as S

runOutputStream :: forall o r a.
                Eff (Output o ': r) a
                -> Eff r (S.Stream (Of o) (Eff r) (), a)
runOutputStream = runState (pure ())
  . reinterpret
    (\case Output o -> modify @(S.Stream (Of o) (Eff r) ()) (S.cons o))

runOutputStream' :: forall o r a.
                Eff (Output o ': r) a
                -> S.Stream (Of o) (Eff r) a
runOutputStream' = undefined
