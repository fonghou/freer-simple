{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall                   #-}

module Control.Monad.Freer.Bracket
  (type Bracketed, liftBracket, runBracket, bracket, finally) where

import Data.Proxy
import GHC.TypeLits
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.OpenUnion
import Data.OpenUnion.Internal
import Control.Monad.Freer.Interpretation
import Control.Monad.Freer.Internal
import Data.Coerce


data Bracket r a where
  Bracket :: KnownNat (Length r')
          => (Eff r' ~> Eff r)
          -> Eff r' a
          -> (a -> Eff r' ())
          -> (a -> Eff r' b)
          -> Bracket r b

type Bracketed r = Eff (Bracket r ': r)


liftBracket :: forall r r'. (Eff r ~> Eff r') -> Bracketed r ~> Bracketed r'
liftBracket f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x -> usingFreer k $ raise $ f $ liftEff x
    Right (Bracket z alloc dealloc doit) ->
      usingFreer k $ send $Bracket (f . z) alloc dealloc doit


raiseLast :: forall m r. Eff r ~> Eff (r :++: '[m])
raiseLast = coerce

liftZoom :: (Eff r ~> Eff '[IO]) -> Eff (r :++: '[m]) ~> Eff '[IO, m]
liftZoom f z = raiseUnder $ f $ coerce z


type family Length (r :: [k]) where
  Length '[] = 0
  Length (a ': r) = 1 + Length r

getLength :: forall k (r :: [k]). KnownNat (Length r) => Word
getLength = fromInteger $ natVal $ Proxy @(Length r)


runBracket :: Bracketed '[IO] ~> IO
runBracket (Freer m) = runResourceT $ m $ \u ->
  case decomp u of
    Left x -> liftIO $ extract x
    Right (Bracket z (alloc :: Eff r' a) dealloc doit) -> do
      let z' :: Eff (r' :++: '[ResourceT IO]) ~> Eff '[ResourceT IO]
          z' = fmap (interpret $ send . liftIO @(ResourceT IO)) $ liftZoom z

          raising :: Eff r' ~> Eff (r' :++: '[ResourceT IO])
          raising = raiseLast @(ResourceT IO)

          liftResource :: ResourceT IO ~> Eff (r' :++: '[ResourceT IO])
          liftResource = liftEff . unsafeInj (getLength @_ @r' + 1)


      runM $ (z') $ do
        a <- raising alloc
        key <- liftResource $ register $ runM $ z $ dealloc a
        r <- raiseLast @(ResourceT IO) $ doit a
        liftResource $ release key
        pure r


bracket
    :: KnownNat (Length r)
    => Eff r a
    -> (a -> Eff r ())
    -> (a -> Eff r b)
    -> Bracketed r b
bracket alloc dealloc doit = send $ Bracket id alloc dealloc doit

finally
  :: KnownNat (Length r)
  => Eff r a
  -> Eff r ()
  -> Bracketed r a
finally act end = bracket (pure ()) (pure end) (const act)
