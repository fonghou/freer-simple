{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.Freer.IO
  ( runEmbedded
  , embedToMonadIO
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Type (Embed (..))
import Control.Monad.IO.Class

-- | Given a natural transform from @m1@ to @m2@
-- run a @Lift m1@ effect by transforming it into a @Lift m2@ effect.
runEmbedded
  :: forall m1 m2 effs a. Member (Embed m2) effs
  => (m1 ~> m2)
  -> Eff (Embed m1 ': effs) a
  -> Eff effs a
runEmbedded f = interpret $ embed . f . unEmbed

------------------------------------------------------------------------------
-- The 'MonadIO' class is conceptually an interpretation of 'IO' to some
-- other monad. This function reifies that intuition, by transforming an 'IO'
-- effect into some other 'MonadIO'.
--
-- This function is especially useful when using the 'MonadIO' instance for
-- 'Eff' instance.
--
-- Make sure to type-apply the desired 'MonadIO' instance when using
-- 'runLiftIO'.
--
-- ==== Example
--
-- @
-- foo :: PandocIO ()
-- foo = 'runM' . 'embedToMonadIO' @PandocIO $ do
--   'liftIO' $ putStrLn "hello from polysemy"
-- @
--
embedToMonadIO
  :: forall m effs a . ( MonadIO m , Member (Embed m) effs)
  => Eff (Embed IO ': effs) a
  -> Eff effs a
embedToMonadIO = runEmbedded $ liftIO @m
{-# INLINE embedToMonadIO #-}
