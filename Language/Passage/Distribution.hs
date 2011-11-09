module Language.Passage.Distribution where

import Language.Passage.AST
import Language.Passage.Term(logGamma, tcase)

logit :: Floating a => a -> a
logit p = log(p/(1-p))

logBeta :: Expr -> Expr -> Expr
logBeta x y = logGamma x + logGamma y - logGamma (x + y)

logFact :: Expr -> Expr
logFact n = logGamma (n + 1)

logComb :: Expr -> Expr -> Expr
logComb n k = logFact n - logFact k - logFact (n - k)

-- | A normal distribution with mean 0 and precision 1
stdNormal :: Distribution
stdNormal = Distribution
  { distName = "N(0,1)"
  , distParams = []
  , distSupport = Real
  , distLL = \x -> -0.5 * x**2
  }

-- | A normal distribution, with a mean and precision
normal :: Expr -> Expr -> Distribution
normal m t = Distribution
  { distName    = "N"
  , distParams  = [m, t]
  , distSupport = Real
  , distLL      =  \x ->   log t        / 2
                         - t * (x ** 2) / 2
                         + t * x * m
                         - t * (m ** 2) / 2
  }

--- | A standard uniform distribution with parameters 0 and 1
standardUniform :: Distribution
standardUniform = Distribution
  { distName    = "SU"
  , distParams  = [0, 1]
  , distSupport = Interval 0 1
  , distLL      = \_ -> 0
  }

--- | A uniform distribution with lower and upper bounds
uniform :: Expr -> Expr -> Distribution
uniform lo hi = Distribution
  { distName    = "U"
  , distParams  = [lo, hi]
  , distSupport = Interval lo hi
    -- NB: Uniform distribution, independent of the variable (hence constant function)
  , distLL      = \_ -> - (log (hi - lo))
  }

discreteUniform :: Expr -> Distribution
discreteUniform n = Distribution
  { distName    = "DisreteUniform"
  , distParams  = [0, n]
  , distSupport = Discrete (Just n)
    -- NB: Uniform distribution, independent of the variable (hence constant function)
  , distLL      = \_ -> - (log (n + 1))
  }

geometric :: Expr -> Distribution
geometric p = Distribution
  { distName    = "Geometric"
  , distParams  = [p]
  , distSupport = Discrete Nothing
  , distLL      = \x -> x * log (1 - p) + log p
  }

-- | A categorical distribution with given support size and probabilities
-- | Probabilities are assumed to add to one (not checked here)
categorical :: Expr -> [Expr] -> Distribution
categorical n ps = Distribution
  { distName    = "Categorical"
  , distParams  = n:ps
  , distSupport = Discrete (Just (n - 1))
  , distLL      = \x -> log (tcase x ps) 
  }

-- | A Bernoulli distribution with a mean
bernoulli :: Expr -> Distribution
bernoulli p = Distribution
  { distName    = "B"
  , distParams  = [p]
  , distSupport = Discrete (Just 1)
  , distLL      = \x -> log (1 - p) + logit p * x
  }

-- | A binomial distribution with given number of samples and probability of success
-- | Number of samples is assumed to be fixed
binomial :: Expr -> Expr -> Distribution
binomial n p = Distribution
  { distName    = "Binomial"
  , distParams  = [n, p]
  , distSupport = Discrete (Just n)
  , distLL      = \x -> logComb n x + x * logit p + n * log (1 - p)
  }

negBinomial :: Expr -> Expr -> Distribution
negBinomial r p = Distribution
  { distName    = "NegativeBinomial"
  , distParams  = [r, p]
  , distSupport = PosReal
  , distLL      = \x -> logComb (x+r-1) x + r * log (1 - p) + x * log p
  }

poisson :: Expr -> Distribution
poisson lambda = Distribution
  { distName    = "Poisson"
  , distParams  = [lambda]
  , distSupport = Discrete Nothing
  , distLL      = \x -> x * log lambda - logFact x - lambda
  }

-- | A beta distribution with the given prior sample sizes.
beta :: Expr -> Expr -> Distribution
beta a b =
  Distribution
    { distName    = "Beta"
    , distParams  = [a, b]
    , distSupport = Interval 0 1
    , distLL      = \x -> (a - 1) * log x + (b - 1) * log (1 - x) - logBeta a b
    }

-- | A gamma distribution with the given prior sample sizes.
dgamma :: Expr -> Expr -> Distribution
dgamma a b =
  Distribution
    { distName    = "Gamma"
    , distParams  = [a, b]
    , distSupport = PosReal
    , distLL      = \x -> a * log b - logGamma a + (a - 1) * log x - b * x
    }

-- | An improper uniform distribution; has no impact on likelihood
improperUniform :: Distribution
improperUniform =
  Distribution
    { distName    = "ImproperUniform"
    , distParams  = []
    , distSupport = Real
    , distLL      = const 0
    }
    
-- | An improper scale
improperScale :: Distribution
improperScale =
  Distribution
    { distName    = "ImproperScale"
    , distParams  = []
    , distSupport = PosReal
    , distLL      = \x -> -log x
    }
