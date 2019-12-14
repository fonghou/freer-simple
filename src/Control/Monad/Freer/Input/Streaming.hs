module Control.Monad.Freer.Input.Streaming
    ( module Control.Monad.Freer.Input
    , runInputStream
    , runInputStream'
    , yieldInput
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Input
import Control.Monad.Freer.State

import Data.Functor.Of

import qualified Streaming as S
import qualified Streaming.Prelude as S

runInputStream :: forall i r a.
               S.Stream (Of i) (Eff r) ()
               -> Eff (Input (Maybe i) ': r) a
               -> Eff r a
runInputStream stream = evalState (Just stream)
  . reinterpret
    (\Input -> do
       get @(Maybe (S.Stream (Of i) (Eff r) ())) >>= \case
         Nothing -> pure Nothing
         Just s  -> raise (S.uncons s) >>= \case
           Nothing      -> do
             put @(Maybe (S.Stream (Of i) (Eff r) ())) Nothing
             pure Nothing
           Just (i, s') -> do
             put $ Just s'
             pure $ Just i)

runInputStream' :: forall i r a.
                (forall x. S.Stream (Of i) (Eff r) x)
                -> Eff (Input i ': r) a
                -> Eff r a
runInputStream' stream = evalState (stream :: S.Stream (Of i) (Eff r) ())
  . reinterpret
    (\Input -> do
      s :: S.Stream (Of i) (Eff r) () <- get
      raise (S.uncons s) >>= \case
        Nothing -> error "runInputStream': impossible!"
        Just (i, s') -> do
          put s'
          pure i)

yieldInput :: Member (Input i) r => S.Stream (Of i) (Eff r) ()
yieldInput = S.lift input >>= S.yield
