{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Error where

import Control.Exception
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.State
import Control.Monad.Freer.Trace
import Data.Function
import GHC.Stack

newtype FooErr = FooErr String deriving (Show)

instance Exception FooErr

test1 :: _ => Eff m ()
test1 = do
  trace "debug"
  i <- input @String
  output "hello"
  output i
  void $ throwError $ FooErr "in test1"
  trace "end"

test1' :: _ => Eff m ()
test1' =
  handleError test1 $
    \(e :: FooErr) -> do
      output ("catched " <> show e)
      throwError "rethrow in test1'"

run1 :: HasCallStack => IO ()
run1 =
  test1'
    & panic @String
    & runInputConst @String "world"
    & outputToTrace id
    & runTrace
    & runM

run1' :: IO ()
run1' = catch run1 $ \(e :: SomeException) -> do
  putStrLn $ displayException e

test2 :: (Members [Input String, Output String, Trace] m) => Eff m ()
test2 = handleError @FooErr test1 $ \e -> output $ show e

run2 :: IO ([String], Either FooErr ())
run2 =
  test2
    & runInputConst @String "howdy"
    & runError @FooErr
    & runOutputList @String
    & runTrace
    & runM

test3 :: Members '[State String, Error FooErr] r => Eff r String
test3 = do
  let throwing
        , catching ::
          Members '[State String, Error FooErr] r => Eff r String
      throwing = do
        modify (++ "-throw")
        void $ throwError $ FooErr "test3"
        get
      catching = do
        modify (++ "-catch")
        get
  catchError @FooErr throwing (\e -> catching >> throwError e)

runError' :: Either FooErr (String, String)
runError' =
  test3 & runState "State before Error" & runError @FooErr & run

runError'' :: (String, Either FooErr String)
runError'' =
  test3 & runError @FooErr & runState "State after Error" & run

decr :: Members '[State Int, Error ()] r => Eff r ()
decr = do
  x <- get @Int
  if x > 0
    then put (pred x)
    else throwError ()

decr3 :: Members '[State Int, Error ()] r => Eff r ()
decr3 = decr >> catchError (decr >> decr) return
