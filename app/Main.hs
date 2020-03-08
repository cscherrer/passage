module Main where

import Control.Monad (zipWithM_)

import Language.Passage (improperUniform, improperScale, normal, genSimulator, setThreadNum, observe, monitor, model,  BayesianNetwork)
import Language.Passage.AST (Expr)

main = putStrLn "hello!"


simple :: Int -> BayesianNetwork (Expr, Expr, [Expr])
simple n = do
  mu <- improperUniform
  tau <- improperScale
  x <- replicate n <$> normal mu tau
  return (mu, tau, x)

simulateSimple :: [Double] -> IO ()
simulateSimple x0 = genSimulator "simple" $ do
  setThreadNum 2
  let n = length x0
  (mu, tau, x) <- model $ simple n
  zipWithM_ observe x x0
  monitor "mean" mu
  monitor "precision" tau
