{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Freer.Type
  (-- * Effect
    Embed (..)
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

newtype Embed m a = Embed { unEmbed :: m a }
  deriving  (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadUnliftIO m => MonadUnliftIO (Embed m) where
  askUnliftIO = Embed $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . unEmbed))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = Embed $ withRunInIO $ \run -> inner (run . unEmbed)
  {-# INLINE withRunInIO #-}
