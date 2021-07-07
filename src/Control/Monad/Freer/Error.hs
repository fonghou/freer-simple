{-# LANGUAGE PatternSynonyms #-}

{- |
 Module:       Control.Monad.Freer.Error
 Description:  An Error effect and handler.
 Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
 License:      BSD3
 Maintainer:   Alexis King <lexi.lambda@gmail.com>
 Stability:    experimental
 Portability:  GHC specific language extensions.

 Composable handler for Error effects. Communicates success\/failure via an
 'Either' type.

 Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
-}
module Control.Monad.Freer.Error (
  Error (..),
  throwError,
  catchError,
  catchJust,
  handleError,
  liftEither,
  liftEitherM,
  mapError,
  runError,
  panic,
  Panic,
  pattern Panic,
) where

import Control.Exception (Exception (..), throwIO)
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Interpretation
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Except as E

import Data.Typeable

import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

-- | Exceptions of the type @e :: *@ with no resumption.
newtype Error e r where
  Error :: e -> Error e r

-- | Throws an error carrying information of type @e :: *@.
throwError :: forall e effs a. Member (Error e) effs => e -> Eff effs a
throwError e = send (Error e)
{-# INLINE throwError #-}

-- | Lift  an 'Either' into an 'Error' effect.
liftEither ::
  forall e effs a. Member (Error e) effs => Either e a -> Eff effs a
liftEither = either throwError return
{-# INLINE liftEither #-}

-- | Lift an 'Either' from the final monad into an 'Error' effect.
liftEitherM ::
  forall e m effs a.
  (Member (Error e) effs, LastMember m effs, Monad m) =>
  m (Either e a) ->
  Eff effs a
liftEitherM = liftEither <=< sendM
{-# INLINE liftEitherM #-}

{- | Handler for exception effects. If there are no exceptions thrown, returns
 'Right'. If exceptions are thrown and not handled, returns 'Left', while
 interrupting the execution of any other effect handlers.
-}
runError :: forall e effs a. Eff (Error e ': effs) a -> Eff effs (Either e a)
runError = shortCircuit $ \(Error e) -> E.throwE e
{-# INLINE runError #-}

-- | A catcher for Exceptions. Handlers are allowed to rethrow exceptions.
catchError ::
  forall e effs a.
  Member (Error e) effs =>
  Eff effs a ->
  (e -> Eff effs a) ->
  Eff effs a
catchError m handle = interposeRelay pure (\(Error e) _ -> handle e) m
{-# INLINE catchError #-}

-- | A catcher for Exceptions. Handlers are /not/ allowed to rethrow exceptions.
handleError ::
  forall e effs a.
  Eff (Error e ': effs) a ->
  (e -> Eff effs a) ->
  Eff effs a
handleError m handle = relay pure (\(Error e) _ -> handle e) m
{-# INLINE handleError #-}

{- | Transform one 'Error' into another. This function can be used to aggregate
 multiple errors into a single type.
-}
mapError ::
  forall e1 e2 r a.
  Member (Error e2) r =>
  (e1 -> e2) ->
  Eff (Error e1 ': r) a ->
  Eff r a
mapError f m = handleError @e1 m $ \e -> throwError (f e)
{-# INLINE mapError #-}

------------------------------------------------------------------------------

{- | The function @'catchJust'@ is like @'catchError'@, but it takes an extra argument
 which is an exception predicate, a function which selects which type of exceptions
 we're interested in.
-}
catchJust ::
  forall e b effs a.
  Member (Error e) effs =>
  (e -> Maybe b) ->
  Eff effs a ->
  (b -> Eff effs a) ->
  Eff effs a
catchJust f m k = catchError m $ \e -> case f e of
  Nothing -> throwError e
  Just x -> k x
{-# INLINE catchJust #-}

{- | Throw an 'Error' as 'Control.Exception' Exception through final monad.

 /Beware/: Effects that aren't interpreted in terms of 'IO'
 will have local state semantics in regards to 'Error' effects
 interpreted this way.
-}
data Panic e = Panic' e CallStack
  deriving (Show)

instance (Typeable e, Show e) => Exception (Panic e) where
  displayException (Panic' e stack) =
    ("Control.Monad.Freer.Error.Panic (" ++)
      . (show e ++)
      . (")\n" ++)
      $ (prettyCallStack stack)

pattern Panic :: e -> Panic e
pattern Panic e <- Panic' e _

panic ::
  forall e m effs a.
  ( HasCallStack
  , LastMember m effs
  , MonadIO m
  , Show e
  , Typeable e
  ) =>
  Eff (Error e ': effs) a ->
  Eff effs a
panic = subsume @m $ \case
  (Error e) -> liftIO $ throwIO $ Panic' e callStack
