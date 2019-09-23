module BracketSpec where

import qualified Control.Exception.Safe as X
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.Error
import Control.Monad.Freer.Trace
import Control.Monad.Freer.Resource

import Test.Hspec

test :: IO Int
test = X.handleAny (\e -> print e >> return 0) $ runResource lower $ do
     bracket
        (trace "Before">> return "!!!")
        (\b -> trace $ "After " <> b)
       $ \b -> do
         trace $ "Begin " <> b
         i <- input @Int
         x <- input @String
         output $ "input is: " <> x
         let ex = mapError @Bool show (throwError False)
         handleError @String ex $ \e -> return e >>= trace . ("Error is " <>)
         _ <- throwError $ "DIE"
         trace "End"
         return (i + 10)
    where
    lower = runTrace
          -- . runTraceList
          . unsafeRunError @String
          . unsafeRunError @Bool
          . outputToTrace @String
          -- . runOutputList @String
          . runInputConst "hello"
          . runInputConst @Int 1

spec :: Spec
spec = return ()
