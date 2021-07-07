module Bad where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

data SomeEff a where
  SomeAction :: SomeEff String

someAction :: Member SomeEff r => Eff r String
someAction = send SomeAction

bad :: Either String String
bad =
  run $
    runError @String $
      interpret (\SomeAction -> throwError "not caught") $
        do
          _ <- someAction
          throwError "caught"
            `catchError` \(e :: String) -> return e

bad2 :: (String, String)
bad2 =
  run $
    runReader "unlocaled" $
      interpret (\SomeAction -> ask) $
        local (const "localed") $ do
          x <- ask
          y <- someAction
          pure (x, y)
