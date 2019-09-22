module BracketSpec where

import Control.Monad.Freer.Input
import Control.Monad.Freer.Error
import Control.Monad.Freer.Trace
import Control.Monad.Freer.Resource

import Test.Hspec

fromRight :: Either a b -> b
fromRight = either undefined id

runTest :: IO ()
runTest = runResource . nt $ do
     bracket_ (trace "before") (trace "after") $ do
       trace "begin"
       x <- input @String
       trace x
       throwError "ERR"
       trace "end"
  where
    nt = liftBracket $ fmap fromRight
         . runError @String
         . runInputConst "hello"
         . runTrace

spec :: Spec
spec = return ()
