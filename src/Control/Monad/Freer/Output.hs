{-# LANGUAGE BangPatterns #-}

module Control.Monad.Freer.Output
    ( Output(..)
    , output
    , outputToWriter
    , ignoreOutput
    , runOutputList
    , runOutputMonoid
    , runOutputMonoidAssocR
    , runOutputMonoidIORef
    , runOutputMonoidTVar
    , runOutputBatched
    , runOutputEff
    ) where

import Control.Concurrent.STM
import Control.Monad ( when )
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Control.Monad.IO.Class

import Data.Bifunctor ( first )
import Data.IORef
import Data.Semigroup ( Endo(..) )

data Output o r where
  Output :: o -> Output o ()

output :: forall o effs. Member (Output o) effs => o -> Eff effs ()
output = send . Output

{-# INLINE output #-}

------------------------------------------------------------------------------
runOutputList :: forall o effs a. Eff (Output o ': effs) a -> Eff effs ([o], a)
runOutputList = fmap (first reverse)
  . runState []
  . reinterpret (\case Output o -> modify' (o:))

{-# INLINE runOutputList #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid.
runOutputMonoid :: forall o m effs a.
                Monoid m
                => (o -> m)
                -> Eff (Output o ': effs) a
                -> Eff effs (m, a)
runOutputMonoid f = runState mempty
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
runOutputMonoidAssocR :: forall o m effs a.
                      Monoid m
                      => (o -> m)
                      -> Eff (Output o ': effs) a
                      -> Eff effs (m, a)
runOutputMonoidAssocR f = fmap (first (`appEndo` mempty))
  . runOutputMonoid (\o -> let !o' = f o
                           in Endo (o' <>))

{-# INLINE runOutputMonoidAssocR #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by ignoring it.
ignoreOutput :: Eff (Output o ': r) a -> Eff r a
ignoreOutput = interpret $ \case Output _ -> pure ()

{-# INLINE ignoreOutput #-}

------------------------------------------------------------------------------
-- | Accumulate 'output's so they are delayed until they reach at least size
-- @size@.
--
-- If @size@ is 0, this interpretation will not emit anything in the resulting
-- 'Output' effect.
runOutputBatched :: forall o r a.
                 Member (Output [o]) r
                 => Int
                 -> Eff (Output o ': r) a
                 -> Eff r a
runOutputBatched 0 m = ignoreOutput m
runOutputBatched size m = do
  ((c, res), a) <- runState (0 :: Int, [] :: [o])
    $ reinterpret (\case Output o -> do
                           (count, acc) <- get
                           let newCount = 1 + count
                               newAcc = o:acc
                           if newCount < size
                             then put (newCount, newAcc)
                             else do
                               output (reverse newAcc)
                               put (0 :: Int, [] :: [o])) m
  when (c > 0) $ output @[o] (reverse res)
  pure a

------------------------------------------------------------------------------
-- | Runs an 'Output' effect by running a monadic action for each of its
-- values.
runOutputEff :: (o -> Eff effs ()) -> Eff (Output o ': effs) a -> Eff effs a
runOutputEff act = interpret $ \case Output o -> act o

{-# INLINE runOutputEff #-}

-- | Transform an 'Output' effect into a 'Writer' effect.
outputToWriter
  :: Member (Writer o) effs => Eff (Output o ': effs) a -> Eff effs a
outputToWriter = interpret $ \case Output o -> tell o

{-# INLINE outputToWriter #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into atomic operations
-- over an 'IORef'.
--
-- @since 1.1.0.0
runOutputMonoidIORef
  :: forall o m t r a.
  (Monoid m, LastMember t r, MonadIO t)
  => IORef m
  -> (o -> m)
  -> Eff (Output o ': r) a
  -> Eff r a
runOutputMonoidIORef ref f = interpret $ \case
  Output o -> sendM $ liftIO $ atomicModifyIORef' ref (\s -> let !o' = f o
                                                             in (s <> o', ()))

{-# INLINE runOutputMonoidIORef #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into atomic operations
-- over a 'TVar'.
runOutputMonoidTVar
  :: forall o m t r a.
  (Monoid m, LastMember t r, MonadIO t)
  => TVar m
  -> (o -> m)
  -> Eff (Output o ': r) a
  -> Eff r a
runOutputMonoidTVar tvar f = interpret $ \case
  Output o -> sendM $ liftIO $ atomically $ do
    s <- readTVar tvar
    writeTVar tvar $! s <> f o

{-# INLINE runOutputMonoidTVar #-}
