module NonDet where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Input
import Control.Monad.Freer.NonDet
import Control.Monad.Freer.Output

select :: Alternative m => [a] -> m a
select = foldr (\x m -> pure x <|> m) empty

test :: Members [Output String, NonDet] r => Eff r ()
test = do
  x <- select [1 :: Int, 2, 3]
  output ("x = " <> show x)
  y <- select [4 :: Int, 5, 6]
  output ("y = " <> show y)

-- runM . runOutputEff (sendM . putStrLn) . runNonDetAll $ test

replicateM' :: MonadPlus m => Int -> m a -> m a
replicateM' n = join . select . replicate n

echo ::
  Members [Input String, Output String, NonDet] r =>
  Int ->
  Eff r ()
echo n = do
  str <- replicateM' n input
  output @String str

-- :{
--   runM
-- . runInputEff (sendM getLine)
-- . runOutputEff (sendM . putStrLn)
-- . runNonDetA @Maybe

-- $ echo 10
-- :}
