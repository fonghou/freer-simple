module Control.Monad.Freer.Input
    ( Input(..)
    , input
    , runInputConst
    , runInputEff
    , runInputList
    , runInputList'
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Fail
import Control.Monad.Freer.State

import Data.Foldable ( for_ )
import Data.List ( uncons )

data Input i a where
  Input :: Input i i

input :: forall i effs. (Member (Input i) effs) => Eff effs i
input = send Input

{-# INLINE input #-}
runInputConst :: i -> Eff (Input i ': effs) a -> Eff effs a
runInputConst c = interpret $ \case Input -> pure c

{-# INLINE runInputConst #-}

------------------------------------------------------------------------------
-- | Runs an 'Input' effect by evaluating a monadic action for each request.
runInputEff
  :: forall i effs a. Eff effs i -> Eff (Input i ': effs) a -> Eff effs a
runInputEff m = interpret $ \case Input -> m

{-# INLINE runInputEff #-}

------------------------------------------------------------------------------
-- | Run an 'Input' effect by providing a different element of a list each
-- time. Returns 'Nothing' after the list is exhausted.
runInputList
  :: forall i effs a. [i] -> Eff (Input (Maybe i) ': effs) a -> Eff effs a
runInputList is = fmap snd . runState is . reinterpret
  (\case Input -> do
                    s <- gets uncons
                    for_ s $ put . snd
                    pure $ fmap fst s)

{-# INLINE runInputList #-}

------------------------------------------------------------------------------
-- | Run an 'Input' effect by providing a different element of a list each
-- time. 'Fail' after the list is exhausted.
runInputList' :: forall i effs a.
              Member Fail effs
              => [i]
              -> Eff (Input i ': effs) a
              -> Eff effs a
runInputList' is = fmap snd . runState is . reinterpret
  (\case Input -> do
                    Just (x, xs) <- gets uncons
                    put xs
                    pure x)

{-# INLINE runInputList' #-}
