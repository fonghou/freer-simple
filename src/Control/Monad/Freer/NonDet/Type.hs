{-# LANGUAGE GADTs #-}

module Control.Monad.Freer.NonDet.Type ( NonDet(..) ) where

data NonDet a where
  Empty :: NonDet a
  Choose :: NonDet Bool
