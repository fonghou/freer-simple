module ResourceSpec where

import Control.Concurrent.STM
import Control.Exception ( ErrorCall(..), try )
import Control.Monad.Freer.Error
import Control.Monad.Freer.Input
import Control.Monad.Freer.Output
import Control.Monad.Freer.Resource

import Data.IORef

import Test.Hspec

spec :: Spec
spec = do
  describe "bracket_" $ do
    it "runs a cleanup action on success (IORef)" $ do
      outputs <- newIORef []
      (result :: Either (ErrorExc ErrorCall) ()) <- try
        . runResource
          (errorToExc @ErrorCall
           . runOutputMonoidIORef @[String] outputs id
           . runInputConst "error")
        $ bracket_ (output ["setup"]) (output ["teardown"])
        $ do
          output ["use"]
          msg <- input @String
          throwError $ ErrorCall msg
      readIORef outputs `shouldReturn` ["setup", "use", "teardown"]
      result `shouldBe` Left (ErrorExc $ ErrorCall "error")
    it "runs a cleanup action on success (TVar)" $ do
      outputs <- newTVarIO []
      (result :: Either (ErrorExc ErrorCall) ()) <- try
        . runResource
          (errorToExc @ErrorCall . runOutputMonoidTVar @[String] outputs id)
        $ bracket_
          (output ["setup"])
          (output ["teardown"])
          (throwError $ ErrorCall "error")
      readTVarIO outputs `shouldReturn` ["setup", "teardown"]
      result `shouldBe` Left (ErrorExc $ ErrorCall "error")
