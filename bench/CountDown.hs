module CountDown (mtl, mtl2, freer, freer2, fused, fused2) where

import qualified Control.Monad.State as MTL
import qualified Control.Monad.Except as MTL

import Control.Monad.Freer
import qualified Control.Monad.Freer.State as Freer
import qualified Control.Monad.Freer.Error as Freer

import Control.Algebra
import qualified Control.Carrier.State.Strict as Eff
import qualified Control.Carrier.Error.Either as Eff

mtl :: MTL.MonadState Int m => m Int
mtl = do
  n <- MTL.get
  if n <= 0 then pure n else MTL.put (n - 1) >> mtl
{-# INLINABLE mtl #-}

mtl2 :: (MTL.MonadState Int m, MTL.MonadError String m)=> m Int
mtl2 = do
  n <- MTL.get
  if n <= 0 then MTL.throwError "wat" else MTL.put (n - 1) >> mtl

freer :: Member (Freer.State Int) eff => Eff eff Int
freer = do
  n <- Freer.get
  if n <= 0 then pure n else Freer.put (n - 1) >> freer
{-# INLINABLE freer #-}

freer2 :: (Member (Freer.State Int) eff, Member (Freer.Error String) eff) => Eff eff Int
freer2 = do
  n <- Freer.get @Int
  if n <= 0 then Freer.throwError "wat" else Freer.put (n - 1) >> freer

fused :: Has (Eff.State Int) sig m => m Int
fused = do
  n <- Eff.get
  if n <= 0 then pure n else Eff.put (n - 1) >> fused
{-# INLINABLE fused #-}

fused2 :: (Has (Eff.State Int) sig m, Has (Eff.Error String) sig m) => m Int
fused2 = do
  n <- Eff.get @Int
  if n <= 0 then Eff.throwError "wat" else Eff.put (n - 1) >> fused
