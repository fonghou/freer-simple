{-# OPTIONS_GHC -ddump-rule-firings -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-ticks -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes #-}

module Main (main) where

-- import qualified Control.Monad.Except as MTL

import Control.Monad.Freer as Freer
import qualified Control.Monad.Freer.State as Freer
import qualified Control.Monad.State as MTL
-- import qualified Control.Monad.Freer.Error as Freer

import CountDown
import Criterion (bench, bgroup, nf)
import Criterion.Main (defaultMain)

countDownMTL :: Int -> (Int, Int)
countDownMTL = MTL.runState mtl

countDownFreer :: Int -> (Int, Int)
countDownFreer start = Freer.run (Freer.runState start freer)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Countdown Bench"
        [ bench "freer.State (inline)" $ nf countDownFreer 10000
        ]
    ]
