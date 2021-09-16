{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Exception as X
import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import Control.Monad.Freer (Eff, Member, runM)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Resource
import Control.Monad.Freer.Trace

import Capitalize (Capitalize, capitalize, runCapitalize)
import Console (
  Console,
  exitSuccess',
  getLine',
  putStrLn',
  runConsoleM,
  runConsolePure,
 )
import Coroutine ()
import Error ()
import Fresh ()
import Trace ()

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------
capitalizingService :: (Member Console r, Member Capitalize r) => Eff r ()
capitalizingService = forever $ do
  putStrLn' "Send something to capitalize..."
  l <- getLine'
  when (null l) exitSuccess'
  capitalize l >>= putStrLn'

-------------------------------------------------------------------------------

mainPure :: IO ()
mainPure =
  print
    . runConsolePure ["cat", "fish", "dog", "bird", ""]
    $ runCapitalize capitalizingService

mainConsoleA :: IO ()
mainConsoleA = runM (runConsoleM (runCapitalize capitalizingService))

--             |     |            |             |
--      IO () -'     |            |             |
--     Eff '[IO] () -'            |             |
--         Eff '[Console, IO] () -'             |
--           Eff '[Capitalize, Console, IO] () -'

mainConsoleB :: IO ()
mainConsoleB = runM (runCapitalize (runConsoleM capitalizingService))

--             |     |              |           |
--      IO () -'     |              |           |
--     Eff '[IO] () -'              |           |
--        Eff '[Capitalize, IO] () -'           |
--           Eff '[Console, Capitalize, IO] () -'

mainBracket :: IO ()
mainBracket =
  X.handle @X.SomeException print $
    runResource (runErrorEx @String . runTrace) $ do
      bracket (trace "alloc") (const $ trace "dealloc") $
        const $ do
          trace "hi"
          _ <- throwError "die"
          trace "bye"

examples :: [(String, IO ())]
examples =
  [ ("pure", mainPure)
  , ("bracket", mainBracket)
  , ("consoleA", mainConsoleA)
  , ("consoleB", mainConsoleB)
  ]

main :: IO ()
main =
  getArgs >>= \case
    [x] -> fromMaybe e $ lookup x examples
    _ -> e
 where
  e = putStrLn msg
  msg = "Usage: prog [" ++ intercalate "|" (map fst examples) ++ "]"
