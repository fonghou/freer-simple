{-# LANGUAGE BangPatterns #-}
module Control.Monad.Freer.Output
  ( Output(..)
  , output
  , runOutputList
  , runOutputMonoid
  , runOutputMonoidAssocR
  , runOutputEff) where

import Data.Semigroup (Endo(..))
import Data.Bifunctor (second)
import Control.Monad.Freer.Internal (Eff, Member, send)
import Control.Monad.Freer.Interpretation
import Control.Monad.Trans.State.Strict (modify')

data Output o a where
  Output :: o -> Output o ()

output :: forall o effs. Member (Output o) effs => o -> Eff effs ()
output = send . Output
{-# INLINE output #-}

runOutputList :: forall o r a.  Eff (Output o ': r) a -> Eff r (a, [o])
runOutputList = (fmap . fmap ) (second reverse)
  $ withStateful [] $ \(Output o) -> modify' (o :)
{-# INLINE runOutputList #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid.
runOutputMonoid
    :: forall o m r a . Monoid m
    => (o -> m)
    -> Eff (Output o ': r) a
    -> Eff r (a, m)
runOutputMonoid f = withStateful mempty $ \(Output o) -> modify' (<> f o)
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
    :: forall o m r a
     . Monoid m
    => (o -> m)
    -> Eff (Output o ': r) a
    -> Eff r (a, m)
runOutputMonoidAssocR f =
    fmap (second (`appEndo` mempty))
  . runOutputMonoid (\o -> let !o' = f o in Endo (o' <>))
{-# INLINE runOutputMonoidAssocR #-}


------------------------------------------------------------------------------
-- | Runs an 'Output' effect by running a monadic action for each of its
-- values.
runOutputEff :: (o -> Eff r ()) -> Eff (Output o ': r) a -> Eff r a
runOutputEff act = interpret $ \case
    Output o -> act o
{-# INLINE runOutputEff #-}

