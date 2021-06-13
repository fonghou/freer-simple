{-# OPTIONS_GHC -ddump-rule-firings -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-ticks -dsuppress-type-applications -dsuppress-unfoldings -dsuppress-uniques -dsuppress-module-prefixes #-}

module Core (main) where

import Control.Monad.Freer as Freer

import qualified Control.Monad.Freer.Error as Freer
import qualified Control.Monad.Freer.State as Freer

-- import qualified Control.Monad.State as MTL
-- import qualified Control.Monad.Except as MTL

import CountDown
import Criterion (bench, bgroup, nf)
import Criterion.Main (defaultMain)

-- countDown :: Int -> (Int, Int)
-- countDown start = Freer.run (Freer.runState start freer)

countDownExc :: Int -> Either String (Int, Int)
countDownExc start = Freer.run $ Freer.runError (Freer.runState start CountDown.freer2)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Countdown Bench"
        [ bench "countDown" $ nf countDownExc 10000
        -- , bench "countDownExc" $ nf countDownExc 10000
        ]
    ]
