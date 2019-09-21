module Control.Monad.Freer.Fail
  ( Fail(..)
  , runFail
  , failToError
  , failToMonadFail
  )
 where

import Control.Monad.Fail as Fail
import Control.Monad.Freer
import Control.Monad.Freer.Internal (Fail(..))
import Control.Monad.Freer.Error

runFail :: Eff (Fail ': r) a -> Eff r (Either String a)
runFail = runError . reinterpret (\(Fail s) -> throwError s)
{-# INLINE runFail #-}

failToMonadFail
  :: forall m r a
  . (LastMember m r, MonadFail m)
  => Eff (Fail ': r) a
  -> Eff r a
failToMonadFail = interpret $ \(Fail s) -> sendM @m (Fail.fail s)

failToError
  :: Member (Error e) r
  => (String -> e)
  -> Eff (Fail ': r) a
  -> Eff r a
failToError f = interpret $ \(Fail s) -> throwError (f s)
{-# INLINE failToError #-}
