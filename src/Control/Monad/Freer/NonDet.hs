module Control.Monad.Freer.NonDet (NonDet, runNonDetA) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad.Freer.NonDet.Type

import Control.Monad.Freer
import Control.Monad.Freer.Interpretation

runNonDetA :: Alternative f => Eff (NonDet ': effs) a -> Eff effs (f a)
runNonDetA = relay (return . pure) $ \m k -> case m of
  Empty -> return empty
  Choose -> liftA2 (<|>) (k True) (k False)
{-# INLINE runNonDetA #-}

{-|
$doctest

>>> import Data.Functor
>>> import Control.Applicative
>>> import Control.Monad
>>> import Control.Monad.Freer.Error

>>> :{
test = do
  i <- msum . fmap pure $ [1..]
  guard (i `mod` 2 ==0)
  return i
:}

>>> run . runNonDetA $ test
2

>>> run . runNonDetA @Maybe $ test
Just 2

>>> take 5 . run . runNonDetA @[] $ test
[2,4,6,8,10]

>>> run . runError . runNonDetA @[] $ (pure 1 <|> throwError ()) `catchError` \() -> pure 2
Right [1,2]
 -}
