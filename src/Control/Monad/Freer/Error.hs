-- |
-- Module:       Control.Monad.Freer.Error
-- Description:  An Error effect and handler.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for Error effects. Communicates success\/failure via an
-- 'Either' type.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Error
  ( Error(..)
  , WrappedError(..)
  , throwError
  , fromEither
  , runError
  , mapError
  , catchError
  , handleError
  , unsafeRunError
  ) where

import qualified Control.Exception as X
import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.Freer.Interpretation
import qualified Control.Monad.Trans.Except as E
import Data.Typeable

-- | Exceptions of the type @e :: *@ with no resumption.
newtype Error e r where
  Error :: e -> Error e r

-- | Throws an error carrying information of type @e :: *@.
throwError :: forall e effs a. Member (Error e) effs => e -> Eff effs a
throwError e = send (Error e)
{-# INLINE throwError #-}

-- | Upgrade an 'Either' into an 'Error' effect.
fromEither :: forall e effs a. Member (Error e) effs
           => Either e a -> Eff effs a
fromEither (Left e) = throwError e
fromEither (Right a) = pure a
{-# INLINE fromEither #-}

-- | Handler for exception effects. If there are no exceptions thrown, returns
-- 'Right'. If exceptions are thrown and not handled, returns 'Left', while
-- interrupting the execution of any other effect handlers.
runError :: forall e effs a. Eff (Error e ': effs) a -> Eff effs (Either e a)
runError = shortCircuit $ \(Error e) -> E.throwE e
{-# INLINE runError #-}

-- | A catcher for Exceptions. Handlers are allowed to rethrow exceptions.
catchError :: forall e effs a . Member (Error e) effs
           => Eff effs a -> (e -> Eff effs a) -> Eff effs a
catchError m handle = interposeRelay pure (\(Error e) _ -> handle e) m
{-# INLINE catchError #-}

-- | A catcher for Exceptions. Handlers are /not/ allowed to rethrow exceptions.
handleError :: forall e effs a. Eff (Error e ': effs) a -> (e -> Eff effs a)
  -> Eff effs a
handleError m handle = relay pure (\(Error e) _ -> handle e) m
{-# INLINE handleError #-}

-- | Transform one 'Error' into another. This function can be used to aggregate
-- multiple errors into a single type.
mapError
  :: forall e1 e2 r a. Member (Error e2) r
   => (e1 -> e2)
   -> Eff (Error e1 ': r) a
   -> Eff r a
mapError f m = handleError @e1 m $ \ e -> throwError (f e)
{-# INLINE mapError #-}


newtype WrappedError e = WrappedError e
  deriving (Typeable)

instance Typeable e => Show (WrappedError e) where
  show = mappend "WrappedError: " . show . typeRep

instance (Typeable e) => X.Exception (WrappedError e)

-- | Run an 'Error' effect as an 'IO' 'X.Exception' through final 'IO'. This
-- interpretation is significantly faster than 'runError'.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Error' effects
-- interpreted this way.
unsafeRunError
  :: (Typeable e, Member IO effs)
  => Eff (Error e ': effs) a -> Eff effs a
unsafeRunError = subsume @IO $ \case
  (Error e) -> X.throwIO $ WrappedError e
{-# INLINE unsafeRunError #-}
