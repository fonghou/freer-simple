-- |
-- Module:       Control.Monad.Freer.NonDet
-- Description:  Non deterministic effects
-- Copyright:    2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'NonDet' effects.
module Control.Monad.Freer.NonDet
  ( NonDet(..)
  , runChoiceA
  ) where

import Control.Applicative ( Alternative(..) )
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Interpretation

runChoiceA :: Alternative f => Eff (NonDet ': effs) a -> Eff effs (f a)
runChoiceA = relay (pure . pure) $ \m k -> case m of
  MZero -> pure empty
  MPlus -> (<|>) <$> k True <*> k False
