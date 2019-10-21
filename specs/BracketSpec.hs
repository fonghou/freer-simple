module BracketSpec where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.Resource
import Control.Monad.Freer.Trace

import qualified UnliftIO.Exception as X

type MyEff r =
  Members '[Input String, Input Int, Output String, Error String, Trace] r

foo :: MyEff r => String -> Eff r Int
foo b = do
  trace $ "Begin " <> b
  i <- input @Int
  x <- input @String
  output $ "input is: " <> x
  let ex = mapError @Bool show (throwError False)
  handleError @String ex $ \e -> return e >>= output . ("Error is " <>)
  _ <- throwError $ "DIE"
  trace "End"
  return (i + 10)

test :: (forall r. MyEff r => String -> Eff r Int) -> IO Int
test f = X.handle (\(ErrorExc (e :: String)) -> print e >> return 0)
  $ runResource lower
  $ bracket (trace "Before" >> return "!!!") (\b -> trace $ "After " <> b) f
  where
    lower = runTrace
      . errorToExc @String
      . outputToTrace @String
      . runInputConst "hello"
      . runInputConst @Int 1
