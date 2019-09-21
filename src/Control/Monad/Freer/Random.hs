{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Freer.Random
  ( -- * Effect
    Random (..)

    -- * Actions
  , random
  , randomR

    -- * Interpretations
  , runRandom
  , runRandomIO
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.TH
import           Control.Monad.Freer.State
import qualified System.Random as R

------------------------------------------------------------------------------
-- | An effect capable of providing 'R.Random' values.
data Random a where
  Random :: R.Random x => Random  x
  RandomR :: R.Random x => (x, x) -> Random x

makeEffect ''Random

------------------------------------------------------------------------------
-- | Run a 'Random' effect with an explicit 'R.RandomGen'.
runRandom
    :: forall q r a
     . R.RandomGen q
    => q
    -> Eff (Random ': r) a
    -> Eff r (q, a)
runRandom q = runState q . reinterpret (\case
  Random -> do
    ~(a, q') <- gets @q R.random
    put q'
    pure a
  RandomR r -> do
    ~(a, q') <- gets @q $ R.randomR r
    put q'
    pure a
                                       )
{-# INLINE runRandom #-}


------------------------------------------------------------------------------
-- | Run a 'Random' effect by using the 'IO' random generator.
runRandomIO :: LastMember IO r => Eff (Random ': r) a -> Eff r a
runRandomIO m = do
  q <- sendM R.newStdGen
  snd <$> runRandom q m
{-# INLINE runRandomIO #-}
