module Control.Monad.Freer.NonDet (NonDet, runNonDet) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad.Freer.NonDet.Type

import Control.Monad.Freer
import Control.Monad.Freer.Interpretation

runNonDet :: Alternative f => Eff (NonDet ': effs) a -> Eff effs (f a)
runNonDet = relay (return . pure) $ \m k -> case m of
  Empty -> return empty
  Choose -> liftA2 (<|>) (k True) (k False)
{-# INLINE runNonDet #-}

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

>>> run . runNonDet $ test
2

>>> run . runNonDet @Maybe $ test
Just 2

>>> take 5 . run . runNonDet @[] $ test
[2,4,6,8,10]

>>> run . runError . runNonDet @[] $ (pure 1 <|> throwError () <|> pure 3) `catchError` \() -> pure 2
Right [1,2,3]

>>> run . runNonDet @[] . runError $ (pure 1 <|> throwError () <|> pure 3) `catchError` \() -> pure 2
[Right 1,Right 2,Right 3]
 -}
