module Bad where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

data SomeEff a where
  SomeAction :: SomeEff String

someAction :: Member SomeEff r => Eff r String
someAction = send SomeAction

badCatch :: Either String String
badCatch =
  run $ runError @String $
    interpret (\SomeAction -> throwError "not caught") $
      do
        _ <- someAction
        throwError "caught"
      `catchError` \(e :: String) -> return e

badLocal :: (String, String)
badLocal =
  run $ runReader "unlocaled" $
    interpret (\SomeAction -> ask) $
      local (const "localed") $ do
        x <- ask
        y <- someAction
        pure (x, y)
