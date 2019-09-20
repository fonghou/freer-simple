{-# LANGUAGE PartialTypeSignatures #-}
module Error where

import Control.Exception.Safe
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
-- import Control.Monad.Freer.State
import Control.Monad.Freer.Trace
import Data.Function

test1 :: (Members [Input String, Output String, Error String, Trace] m)
      => Eff m ()
test1 = do
  trace "debug"
  i <- input @String
  output "hello"
  output $ i
  throwError "DIE"
  trace "end"

run1 :: IO ()
run1 = test1
  & runInputConst @String "world"
  & unsafeThrowError @String
  & outputToTrace @String
  & runTrace
  & runM

run1' :: IO ()
run1' = catchAny run1 $ \s -> do
  print s

test2 :: (Members [Input String, Output String, Trace] m)
      => Eff m ()
test2 = handleError @String test1 $ \e -> output e

run2 :: IO _
run2 = test2
  & runInputConst @String "howdy"
  & runError @String
  & runOutputList @String
  & runTrace
  & unsafeThrowError @String
  & runM
