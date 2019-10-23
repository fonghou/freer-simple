module BracketSpec where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.Resource
import Control.Monad.Freer.Trace

import qualified UnliftIO.Exception as X

-- import Test.Hspec
--
-- spec :: Spec
-- spec = return ()

type MyEff r =
  Members' [Input String, Input Int, Output String, Error String, Trace] r

foo :: (MyEff r) => Bracketed r Int
foo = bracket (trace "Before" >> return "!!!") (\b -> trace $ "After " <> b)
  $ \b -> do
    trace $ "Begin " <> b
    i <- input @Int
    x <- input @String
    output $ "input is: " <> x
    let ex = mapError @Bool show (throwError False)
    handleError @String ex $ \e -> return e >>= output . ("Error is " <>)
    _ <- throwError $ "DIE"
    trace "End"
    return (i + 10)

test :: IO Int
test = X.handle (\(ErrorExc (e :: String)) -> print e >> return 0)
  $ runResource lower foo
  where
    lower = runTrace
      . errorToExc @String
      . outputToTrace @String
      . runInputConst "hello"
      . runInputConst @Int 1
