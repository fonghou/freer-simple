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

test1 :: (Members [Input Int, Output String, Error String, Trace] m)
      => Eff m ()
test1 = do
  trace "debug"
  i <- input @Int
  output "hello"
  output $ show i
  throwError "DIE"
  trace "end"

run1 :: IO _
run1 = test1
  & runInputConst @Int 1
  & unsafeThrowError @String
  & outputToTrace @String
  & runTrace
  & runM

run1' :: IO _
run1' = catchAny run1 $ \s -> do
  print s

test2 :: (Members [Input Int, Output String, Trace] m)
      => Eff m ()
test2 = handleError @String test1 $ \e -> output e

run2 :: IO _
run2 = test2
  & runInputConst @Int 1
  & runError @String
  & runOutputList @String
  & runTrace
  & unsafeThrowError @String
  & runM
