{-# LANGUAGE PartialTypeSignatures #-}

module Error where

import Control.Exception.Safe
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.State
import Control.Monad.Freer.Trace
import Control.Monad.Freer.Writer

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
   & errorToExc @String
   & outputToTrace @String
   & runTrace
   & runM

run1' :: IO ()
run1' = catch run1 $ \(ErrorExc (s::String)) -> do
  print s

test2 :: (Members [Input String, Output String, Trace] m) => Eff m ()
test2 = handleError @String test1 $ \e -> output e

run2 :: IO _
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

run3 :: IO ()
run3 = test3
   & runError
   & evalState "Error before State"
   & fmap (either id id)
   & runM
   & (print =<<)

run3' :: IO ()
run3' = test3
   & evalState "State before Error"
   & runError
   & fmap (either id id)
   & runM
   & (print =<<)

writer1 :: (String, (String, ()))
writer1 = run . runWriter $ listen $ tell "yup"

writer2 :: (String, ())
writer2 = run . runWriter $ censor @String (const "hello") $ tell "yup"

