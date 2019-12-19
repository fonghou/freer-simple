{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Provides combinators for the lens-based manipulation of state and
-- reader types.
module Control.Monad.Freer.Lens
    ( view
    , views
    , use
    , uses
    , assign
    , modifying
    , catching
    , catching_
    , trying
    , (.=)
    , (%=)
    , (+=)
    , (-=)
    , (*=)
    , (//=)
    , (<>=)
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader as Reader
import Control.Monad.Freer.State as State
import Control.Monad.Freer.Error (Error, catchJust)

import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import Lens.Micro.Type ( ASetter, Getting )

import qualified Data.Monoid as M

view :: forall r a eff. (Member (Reader r) eff) => Getting a r a -> Eff eff a
view l = Reader.asks @r (Lens.view l)
{-# INLINE view #-}

views :: forall r a b eff.
      (Member (Reader r) eff)
      => Getting a r a
      -> (a -> b)
      -> Eff eff b
views l f = fmap f (Reader.asks @r (Lens.view l))
{-# INLINE views #-}

use :: forall s a eff. (Member (State s) eff) => Getting a s a -> Eff eff a
use l = State.gets @s (Lens.view l)
{-# INLINE use #-}

uses :: forall s a b eff.
     (Member (State s) eff)
     => Getting a s a
     -> (a -> b)
     -> Eff eff b
uses l f = fmap f (State.gets @s (Lens.view l))
{-# INLINE uses #-}

assign, (.=) :: forall s a b eff.
             (Member (State s) eff)
             => ASetter s s a b
             -> b
             -> Eff eff ()
assign l b = State.modify @s (Lens.set l b)
{-# INLINE assign #-}

modifying, (%=) :: forall s a b eff.
                (Member (State s) eff)
                => ASetter s s a b
                -> (a -> b)
                -> Eff eff ()
modifying l f = State.modify @s (Lens.over l f)
{-# INLINE modifying #-}

infixr 4 .=
(.=) = assign
{-# INLINE (.=) #-}


infixr 4 %=
(%=) = modifying
{-# INLINE (%=) #-}

infix 4 +=
(+=) :: (Member (State s) eff, Num a) => ASetter s s a a -> a -> Eff eff ()
l += x = l %= (+ x)
{-# INLINE (+=) #-}

infix 4 -=
(-=) :: (Member (State s) eff, Num a) => ASetter s s a a -> a -> Eff eff ()
l -= x = l %= subtract x
{-# INLINE (-=) #-}

infix 4 *=
(*=) :: (Member (State s) eff, Num a) => ASetter s s a a -> a -> Eff eff ()
l *= x = l %= (* x)
{-# INLINE (*=) #-}

infix 4 //=
(//=)
  :: (Member (State s) eff, Fractional a) => ASetter s s a a -> a -> Eff eff ()
l //= x = l %= (/ x)
{-# INLINE (//=) #-}

infix 4 <>=
(<>=) :: (Member (State s) eff, Monoid a) => ASetter s s a a -> a -> Eff eff ()
l <>= x = l %= (<> x)
{-# INLINE (<>=) #-}

catching :: forall e a eff r.
           Member (Error e) eff
           => Getting (M.First a) e a
           -> Eff eff r
           -> (a -> Eff eff r)
           -> Eff eff r
catching l = catchJust (Lens.preview l)
{-# INLINE catching #-}

catching_ :: forall e a eff r.
           Member (Error e) eff
           => Getting (M.First a) e a
           -> Eff eff r
           -> Eff eff r
           -> Eff eff r
catching_ l a b = catchJust (Lens.preview l) a (const b)
{-# INLINE catching_ #-}

trying :: Member (Error e) eff
       => Getting (M.First a) e a
       -> Eff eff r
       -> Eff eff (Either a r)
trying l m = catching l (fmap Right m) (return . Left)
{-# INLINE trying #-}
