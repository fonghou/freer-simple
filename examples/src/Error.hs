{-# LANGUAGE PartialTypeSignatures #-}

module Error where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.State
import Control.Monad.Freer.Trace

import Data.Function

import UnliftIO.Exception as X

test1
  :: (Members [Input String, Output String, Error String, Trace] m) => Eff m ()
test1 = do
  trace "debug"
  i <- input @String
  output "hello"
  output i
  throwError "DIE"
  trace "end"

run1 :: IO ()
run1 = test1
  & runInputConst @String "world"
  & errorToExc @String
  & outputToTrace id
  & runTrace
  & runM

run1' :: IO ()
run1' = catch run1 $ \(ErrorExc (s :: String)) -> do
  print s

test2 :: (Members [Input String, Output String, Trace] m) => Eff m ()
test2 = handleError @String test1 $ \e -> output e

run2 :: IO ([String], Either String ())
run2 = test2
  & runInputConst @String "howdy"
  & runError @String
  & runOutputList @String
  & runTrace
  & errorToExc @String
  & runM

test3 :: Members '[State String, Error String] r => Eff r String
test3 = do
  let throwing, catching
        :: Members '[State String, Error String] r => Eff r String
      throwing = do
        modify (++ "-throw")
        throwError "error"
        get
      catching = do
        modify (++ "-catch")
        get
  catchError @String throwing (\_ -> catching)

run3 :: String
run3 =
  test3 & runError & evalState "Error before State" & fmap (either id id) & run

run3' :: String
run3' =
  test3 & evalState "State before Error" & runError & fmap (either id id) & run

decr :: Members '[State Int, Error ()] r => Eff r ()
decr = do
  x <- get @Int
  if x > 0
    then put (pred x)
    else throwError ()

decr3 :: Members '[State Int, Error ()] r => Eff r ()
decr3 = decr >> catchError (decr >> decr) return
