module OutputSpec where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception ( evaluate )
import Control.Monad.Freer
import Control.Monad.Freer.Output

import Data.Foldable
import Data.Functor.Identity

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "runOutputBatched" $ do
    it "should return nothing at batch size 0" $ do
      let (ms, _) = runOutput 0 $ traverse (output @Int) [0 .. 99]
      length ms `shouldBe` 0
    for_ (1:[5 .. 13] ++ [99 .. 101])
      $ \size -> context ("Works at size " ++ show size) $ do
        let (ms, _) = runOutput size $ traverse (output @Int) [0 .. 99]
        it "returns the correct amount of batches"
          $ length ms
          `shouldBe` div (100 + size - 1) size -- 100 `div` size but rounding up
        it "all batches except the last one are of the specified size"
          $ map length (init ms) `shouldBe` replicate (length ms - 1) size
        it "returns all original elements in the correct order"
          $ concat ms `shouldBe` [0 .. 99]
  describe "runOutputList"
    $ it "should return elements in the order they were output"
    $ let (xs, ()) = runOutputList' $ traverse_ (output @Int) [0 .. 100]
      in xs `shouldBe` [0 .. 100]
  describe "runOutputMonoid"
    $ it "should be strict in the output"
    $ let t = runOutputMonoid (id @String) $ do
            output @String (error "strict")
            return ()
      in do
           runM t `shouldThrow` errorCall "strict"
           evaluate (run t) `shouldThrow` errorCall "strict"
  describe "runOutputMonoidTVar" $ do
    it "should commit writes of asynced computations"
      $ let io = do
              ref <- newTVarIO ""
              concurrently_
                (runM . runOutputMonoidTVar ref (show @Int) $ test1)
                (runM . runOutputMonoidTVar ref (show @Int) $ test1)
              readTVarIO ref
        in do
             res <- io
             res `shouldBe` "1212"

runOutput :: Int -> Eff '[Output Int, Output [Int], Identity] a -> ([[Int]], a)
runOutput size = run . runOutputMonoid (:[]) . runOutputBatched size

runOutputList' :: Eff '[Output Int, Identity] a -> ([Int], a)
runOutputList' = run . runOutputList

test1 :: Members '[Output Int, IO] r => Eff r ()
test1 = do
  output @Int 1
  output @Int 2
  return ()
