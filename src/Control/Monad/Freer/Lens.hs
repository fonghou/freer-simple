{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- | Provides combinators for the lens-based manipulation of state and
-- reader types.
module Control.Monad.Freer.Lens
  ( view, views
  , use, uses
  , assign, (.=)
  , modifying, (%=)
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader as Reader
import Control.Monad.Freer.State as State
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import Lens.Micro.Type (ASetter, Getting)

view
  :: forall r a eff. (Member (Reader r) eff)
  => Getting a r a -> Eff eff a
view l = Reader.asks @r (Lens.view l)
{-# INLINE view #-}

views
  :: forall r a b eff. (Member (Reader r) eff)
  => Getting a r a -> (a -> b) -> Eff eff b
views l f = fmap f (Reader.asks @r (Lens.view l))
{-# INLINE views #-}

use
  :: forall s a eff. (Member (State s) eff)
  => Getting a s a -> Eff eff a
use l = State.gets (Lens.view l)
{-# INLINE use #-}

uses
  :: forall s a b eff. (Member (State s) eff)
  => Getting a s a -> (a -> b) ->  Eff eff b
uses l f = fmap f (State.gets @s (Lens.view l))
{-# INLINE uses #-}

infixr 4 .=
(.=) = assign

assign, (.=)
  :: forall s a b eff. (Member (State s) eff)
  => ASetter s s a b -> b -> Eff eff ()
assign l b = State.modify' (Lens.set l b)
{-# INLINE assign #-}

infixr 4 %=
(%=) = modifying

modifying, (%=)
  :: forall s a b eff. (Member (State s) eff)
  => ASetter s s a b -> (a -> b) -> Eff eff ()
modifying l f = State.modify' (Lens.over l f)
{-# INLINE modifying #-}
