module Coroutine () where

import Control.Monad.Freer
import Control.Monad.Freer.Coroutine
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Trace


-- First example of coroutines
yieldInt :: Member (Yield Int ()) r => Int -> Eff r ()
yieldInt i = yield i id

th1 :: Member (Yield Int ()) r => Eff r ()
th1 = yieldInt 1 >> yieldInt 2

c1 :: Eff '[IO] ()
c1 = runTrace $ runC th1 >>= loop
 where loop (Continue x k) = trace (show (x::Int)) >> k () >>= loop
       loop (Done _)    = trace "Done"
{-
1
2
Done
-}

-- Add dynamic variables
-- The code is essentially the same as that in transf.hs (only added
-- a type specializtion on yield). The inferred signature is different though.
-- Before it was
--    th2 :: MonadReader Int m => CoT Int m ()
-- Now it is more general:
th2 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th2 = ask >>= yieldInt >> (ask >>= yieldInt)

-- Code is essentially the same as in transf.hs; no liftIO though
c2 :: Eff '[IO] ()
c2 = runTrace $ runReader (10::Int) (loop =<< runC th2)
 where loop (Continue x k) = trace (show (x::Int)) >> k () >>= loop
       loop (Done _)    = trace "Done"
{-
10
10
Done
-}

-- locally changing the dynamic environment for the suspension
c21 :: Eff '[IO] ()
c21 = runTrace $ runReader (10::Int) (loop =<< runC th2)
 where loop (Continue x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop (Done _)   = trace "Done"
{-
10
11
Done
-}

-- Real example, with two sorts of local rebinding
th3 :: (Member (Yield Int ()) r, Member (Reader Int) r) => Eff r ()
th3 = ay >> ay >> local (+(10::Int)) (ay >> ay)
 where ay = ask >>= yieldInt

c3 :: Eff '[IO] ()
c3 = runTrace $ runReader (10::Int) (loop =<< runC th3)
 where loop (Continue x k) = trace (show (x::Int)) >> k () >>= loop
       loop (Done _)   = trace "Done"
{-
10
10
20
20
Done
-}

-- locally changing the dynamic environment for the suspension
c31 :: Eff '[IO] ()
c31 = runTrace $ runReader  (10::Int) (loop =<< runC th3)
 where loop (Continue x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop (Done _)   = trace "Done"
{-
10
11
21
21
Done
-}
-- The result is exactly as expected and desired: the coroutine shares the
-- dynamic environment with its parent; however, when the environment
-- is locally rebound, it becomes private to coroutine.

-- We now make explicit that the client computation, run by th4,
-- is abstract. We abstract it out of th4
c4 :: Eff '[IO] ()
c4 = runTrace $ runReader (10::Int) (loop =<< runC (th4 client))
 where loop (Continue x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop (Done _)   = trace "Done"

       -- cl, client, ay are monomorphic bindings
       th4 cl = cl >> local (+(10::Int)) cl
       client = ay >> ay
       ay     = ask >>= yieldInt

{-
10
11
21
21
Done
-}

-- Even more dynamic example
c5 :: Eff '[IO] ()
c5 = runTrace $ runReader (10::Int) (loop =<< runC (th client))
 where loop (Continue x k) = trace (show (x::Int)) >> local (\_->x+1) (k ()) >>= loop
       loop (Done _)   = trace $ "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= yieldInt

       -- There is no polymorphic recursion here
       th cl = do
         _ <- cl
         v <- ask
         _ <- (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
10
11
12
18
18
18
29
29
29
29
29
29
Done
-}

-- And even more
c7 :: Eff '[IO] ()
c7 = runTrace . runReader (1000::Double) . runReader (10::Int)
   $ runC (th client) >>= loop
 where loop (Continue x k) = trace (show (x::Int)) >>
                      local (\_->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop (Done _)    = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         _ <- cl
         v <- ask
         _ <- (if v > (20::Int) then id else local (+(5::Int))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)

{-
1010
1021
1032
1048
1064
1080
1101
1122
1143
1169
1195
1221
1252
1283
1314
1345
1376
1407
Done
-}

c7' :: Eff '[IO] ()
c7' = runTrace . runReader (1000::Double) . runReader (10::Int)
    $ runC (th client) >>= loop
 where loop (Continue x k) = trace (show (x::Int)) >>
                      local (\_->fromIntegral (x+1)::Double) (k ()) >>= loop
       loop (Done _)   = trace "Done"

       -- cl, client, ay are monomorphic bindings
       client = ay >> ay >> ay
       ay     = ask >>= \x -> ask >>=
                 \y -> yieldInt (x + round (y::Double))

       -- There is no polymorphic recursion here
       th cl = do
         _ <- cl
         v <- ask
         _ <- (if v > (20::Int) then id else local (+(5::Double))) cl
         if v > (20::Int) then return () else local (+(10::Int)) (th cl)
{-
1010
1021
1032
1048
1048
1048
1069
1090
1111
1137
1137
1137
1168
1199
1230
1261
1292
1323
Done
-}

