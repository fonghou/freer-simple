{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Monad.Freer.Fail
  ( Fail(..)
  , runFail
  , failToError
  , failToMonad
  , failToEmbed
  )
 where

import Control.Monad.Fail as Fail
import Control.Monad.Freer
import Control.Monad.Freer.Internal (Fail(..))
import Control.Monad.Freer.Error

runFail :: Eff (Fail ': r) a -> Eff r (Either String a)
runFail = runError . reinterpret (\(Fail s) -> throwError s)
{-# INLINE runFail #-}

failToMonad
  :: forall m r a . (LastMember m r, MonadFail m)
  => Eff (Fail ': r) a
  -> Eff r a
failToMonad = interpret $ \(Fail s) -> sendM @m (Fail.fail s)

failToError
  :: Member (Error e) r
  => (String -> e)
  -> Eff (Fail ': r) a
  -> Eff r a
failToError f = interpret $ \(Fail s) -> throwError (f s)
{-# INLINE failToError #-}

-- | Run a 'Fail' effect in terms of an underlying 'MonadFail' instance.
failToEmbed
  :: forall m r a. (Member (Embed m) r, MonadFail m)
  => Eff (Fail ': r) a -> Eff r a
failToEmbed = interpret $ \(Fail s) -> embed @m (Fail.fail s)
{-# INLINE failToEmbed #-}

{-|
$doctest

>>> :{
test :: Member Fail r => Maybe Bool -> Eff r Bool
test mb = do
  Just b <- pure mb
  pure b
:}

>>> runM @[] . failToMonad $ test Nothing
[]

>>> runM @Maybe . failToMonad $ test Nothing
Nothing

>>> runM @Maybe . failToMonad $ test (Just False)
Just False

>>> run . runError @String . failToError id $ test Nothing
Left "Pattern match failure in do expression at <interactive>:24:3-8"

>>> run . runError @String . failToError id $ test (Just True)
Right True

>>> run . runError @String . failToError id $ test (Just False)
Right False

-}
