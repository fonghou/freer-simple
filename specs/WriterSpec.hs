{-# LANGUAGE AllowAmbiguousTypes #-}
module WriterSpec where

import Test.Hspec

import Control.Exception (evaluate)

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer

-- censor' :: forall e s a r
--         . (Member (Error e) r, Member (Writer s) r)
--         => (s -> s)
--         -> Eff r a
--         -> Eff r a
-- censor' f m = do
--   res <- censor f $ fmap Right m `catchError` (pure . Left)
--   case res of
--       Right res' -> return res'
--       Left e -> throwError (e :: e)

test1 :: (String, ())
test1 =
    run
  . runWriter $
  do
    tell "censoring"
    censor @String
      (drop 4)
      (tell " not applied")

test2 :: (String, Either () ())
test2 =
    run
  . runWriter
  . runError $
  do
    tell "censoring"
    censor @String
      (drop 4)
      (tell " not applied" *> throwError ())
    `catchError`
      (\(_ :: ()) -> pure ())

-- test2 :: (String, Either () ())
-- test2 =
--     run
--   . runWriter
--   . runError $
--   do
--     tell "censoring"
--     censor' @() @String
--       (drop 4)
--       (tell " not applied" *> throwError ())
--     `catchError`
--       (\(_ :: ()) -> pure ())

test3 :: (String, (String, ()))
test3 = run . runWriter $ listen (tell "and hear")


spec :: Spec
spec = do
  describe "writer" $ do
    it "should censor" $ do
      test1 `shouldBe` ("censoring applied", ())

    it "should not censor" $ do
      test2 `shouldBe` ("censoring", Right ())

    it "should have a proper listen" $ do
      test3 `shouldBe` ("and hear", ("and hear", ()))

    it "should be strict in the output" $
      let
        t1 = runWriter @String $ do
          tell @String (error "strict")
          return ()

        t2 = runWriter @String $ do
          _ <- listen @String (tell @String (error "strict"))
          return ()

        t3 = runWriter @String $ do
          pass @String $ pure (\_ -> error "strict", ())
          return ()
      in do
        runM t1           `shouldThrow` errorCall "strict"
        evaluate (run t1) `shouldThrow` errorCall "strict"
        runM t2           `shouldThrow` errorCall "strict"
        evaluate (run t2) `shouldThrow` errorCall "strict"
        runM t3           `shouldThrow` errorCall "strict"
        evaluate (run t3) `shouldThrow` errorCall "strict"

