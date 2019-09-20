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
  output "hello"
  throwError "DIE"

run1 :: IO ((), [String])
run1 = test1
  & runInputConst @Int 1
  & runOutputList @String
  & unsafeThrowError @String
  & runTrace
  & runM

run1' :: IO ((), [String])
run1' = catchAny run1 $ \s -> do
  return ((), [ (show s) ])

run2 :: IO ( Either String ((), [String]) )
run2 = test1
  & runInputConst @Int 1
  & runOutputList @String
  & runError @String
  & unsafeThrowError @String
  & runTrace
  & runM
