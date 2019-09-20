{-# LANGUAGE BangPatterns #-}
module Control.Monad.Freer.Output
  ( Output(..)
  , output
  , outputToWriter
  , runOutputList
  , runOutputMonoid
  , runOutputMonoidAssocR
  , runOutputEff) where

import Data.Semigroup (Endo(..))
import Data.Bifunctor (second)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

data Output o r where
  Output :: o -> Output o ()

output :: forall o effs. Member (Output o) effs => o -> Eff effs ()
output = send . Output
{-# INLINE output #-}

runOutputList :: forall o effs a.  Eff (Output o ': effs) a -> Eff effs (a, [o])
runOutputList =
  fmap (second reverse)
  . runState []
  . reinterpret (\case Output o -> modify' (o :))
{-# INLINE runOutputList #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid.
runOutputMonoid
    :: forall o m effs a . Monoid m
    => (o -> m)
    -> Eff (Output o ': effs) a
    -> Eff effs (a, m)
runOutputMonoid f =
  runState mempty
  . reinterpret (\case Output o -> modify' (<> f o))
{-# INLINE runOutputMonoid #-}

------------------------------------------------------------------------------
-- | Like 'runOutputMonoid', but right-associates uses of '<>'.
--
-- This asymptotically improves performance if the time complexity of '<>' for
-- the 'Monoid' depends only on the size of the first argument.
--
-- You should always use this instead of 'runOutputMonoid' if the monoid
-- is a list, such as 'String'.
runOutputMonoidAssocR
    :: forall o m effs a
     . Monoid m
    => (o -> m)
    -> Eff (Output o ': effs) a
    -> Eff effs (a, m)
runOutputMonoidAssocR f =
    fmap (second (`appEndo` mempty))
  . runOutputMonoid (\o -> let !o' = f o in Endo (o' <>))
{-# INLINE runOutputMonoidAssocR #-}


------------------------------------------------------------------------------
-- | Runs an 'Output' effect by running a monadic action for each of its
-- values.
runOutputEff :: (o -> Eff effs ()) -> Eff (Output o ': effs) a -> Eff effs a
runOutputEff act = interpret $ \case
    Output o -> act o
{-# INLINE runOutputEff #-}

-- | Transform an 'Output' effect into a 'Writer' effect.
outputToWriter :: Member (Writer o) effs => Eff (Output o ': effs) a -> Eff effs a
outputToWriter = interpret $ \case
  Output o -> tell o
{-# INLINE outputToWriter #-}
