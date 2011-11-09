module Language.Passage.UI where

import qualified Language.Passage.Distribution as D
import Language.Passage.AST
import Control.Monad

-- | A normal distribution, with a mean and precision
normal :: Expr -> Expr -> BayesianNetwork Expr
normal m t = using (D.normal m t)

--- | A standard uniform distribution with parameters 0 and 1
standardUniform :: BayesianNetwork Expr
standardUniform = using D.standardUniform

--- | A uniform distribution with lower and upper bounds
uniform :: Expr -> Expr -> BayesianNetwork Expr
uniform lo hi = using (D.uniform lo hi)

-- | A Bernoulli distribution with a mean
bernoulli :: Expr -> BayesianNetwork Expr
bernoulli t = using (D.bernoulli t)

categorical n ps = using (D.categorical n ps)

-- | A beta distribution with the given prior sample sizes.
beta :: Expr -> Expr -> BayesianNetwork Expr
beta a b = using (D.beta a b)

-- | A gamma distribution with the given prior sample sizes.
dgamma :: Expr -> Expr -> BayesianNetwork Expr
dgamma a b = using (D.dgamma a b)

-- | A chi-square distribution with the given degrees of freedom.
chiSquare :: Expr -> BayesianNetwork Expr
chiSquare df = dgamma (0.5*df) 0.5

-- | An exponential distribution with the given rate (inverse scale)
dexp :: Expr -> BayesianNetwork Expr
dexp lambda = dgamma 1 lambda

-- | A Student's T distribution, given the degrees of freedom.
studentT :: Expr -> BayesianNetwork Expr
studentT df = do
  v <- chiSquare df
  normal 0 v

symDirichlet :: Int -> Expr -> BayesianNetwork [Expr]
symDirichlet n alpha = do
  gs <- replicateM n $ dgamma alpha 1
  let s = sum gs
  return [g/s | g <- gs]

-- | An improper uniform distribution; has no impact on likelihood
improperUniform :: BayesianNetwork Expr
improperUniform = using D.improperUniform

improperScale :: BayesianNetwork Expr
improperScale = using D.improperScale
