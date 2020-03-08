{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Passage.SimulatorConf where

import MonadLib
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Language.Passage.Term
import Language.Passage.AST
import Language.Passage.Utils
import Language.Passage.Graph

-- The "simulator" infrastructure
data SimState = SimState
  { cfgSampleNum    :: Int
  , cfgItsPerSample :: Int
  , cfgWarmup       :: Int
  , cfgMersenne     :: Bool
  , cfgProfile      :: Bool
  , cfgMonitor      :: [(String, Term NodeIdx)]
  , cfgObserve      :: IM.IntMap Double   -- NodeIdx |-> observed values
  , cfgInitialize   :: IM.IntMap Double
  , cfgRandomSeed   :: [Int]      -- reversed list of seeds for threads
  , cfgNetwork      :: Maybe BayesianGraph
  , cfgThreadNum    :: Int
  , cfgSpecialSlicers :: Bool
  , cfgSplitFiles    :: Bool
  }

initSimState :: SimState
initSimState = SimState { cfgSampleNum    = 100000
                        , cfgItsPerSample = 10
                        , cfgWarmup       = 1000
                        , cfgMersenne     = False
                        , cfgProfile      = False
                        , cfgMonitor      = []
                        , cfgObserve      = IM.empty
                        , cfgInitialize   = IM.empty
                        , cfgRandomSeed   = []
                        , cfgNetwork      = Nothing
                        , cfgThreadNum    = 2
                        , cfgSpecialSlicers = False
                        , cfgSplitFiles     = False
                        }


newtype BayesianSimulator a = B (StateT SimState Id a)
                                      deriving (Functor, Applicative, Monad)

upd :: (SimState -> SimState) -> BayesianSimulator ()
upd f = B $ sets_ f

getField :: (SimState -> a) -> BayesianSimulator a
getField f = B (f `fmap` get)

runSim :: BayesianSimulator a -> SimState
runSim (B m) = snd $ runId $ runStateT initSimState m

setWarmupCount  :: Int -> BayesianSimulator ()
setWarmupCount i = upd $ \s -> s { cfgWarmup = i }

setSampleCount :: Int -> BayesianSimulator ()
setSampleCount i = upd $ \s -> s { cfgSampleNum = i }

setIterationsPerSample :: Int -> BayesianSimulator ()
setIterationsPerSample i = upd $ \s -> s { cfgItsPerSample = i }

-- | Set the random seed for a thread.  This function may be calledd
-- multiple times to set the seeds for multiple threads.
-- The seeds are used in order: first call is for thread 0, next for thread 1,
-- etc.
setRandomSeed :: Int -> BayesianSimulator ()
setRandomSeed d = upd $ \s -> s { cfgRandomSeed = d : cfgRandomSeed s }

setThreadNum :: Int ->  BayesianSimulator ()
setThreadNum n = upd $ \s -> s { cfgThreadNum = n }

useMersenneTwister :: Bool -> BayesianSimulator ()
useMersenneTwister b = upd (\s -> s { cfgMersenne = b })

-- | When using a specialized slizer, we generate a custom slicer
-- for each stochastic variable.  The benefit of this is that, in principle,
-- this may result in more efficient code, at the cost of longer compilation
-- time, and larger binary.  The alternative is to use a generic slicing
-- function which is parameterized by the log-likelihood function for
-- a variable.
useSpecialSlicers :: Bool -> BayesianSimulator ()
useSpecialSlicers b = upd (\s -> s { cfgSpecialSlicers = b })

-- | Generate a separate file for each stochastic variable.
-- The benefit of this flag is that it makes it possible to compile
-- multiple files in parallel.  The drawback is that some optimizations
-- may be lost because the files are compiled separately.  Also, there
-- is some overhead for processing multiple files.
splitFiles :: Bool -> BayesianSimulator ()
splitFiles b = upd (\s -> s { cfgSplitFiles = b })

enableProfiling :: Bool -> BayesianSimulator ()
enableProfiling b = upd $ \s -> s { cfgProfile = b }

model :: BayesianNetwork a -> BayesianSimulator a
model t = do upd $ \s -> s { cfgNetwork = Just nw }
             return a
  where (a, nw) = buildBayesianGraph t

observe :: Term NodeIdx -> Double -> BayesianSimulator ()
observe t d =
  case splitArray t of
    -- (TVar idx, its) | Just is <- mapM isConst its
    (TVar idx, []) ->
      do obs <- getField cfgObserve
         case IM.insertLookupWithKey unused idx d obs of
           (Nothing, m1) -> upd (\s -> s { cfgObserve = m1 })
           (Just _, _)   ->
              error $ "observe: Model error. Node was observed before: "
                                                                  ++ show t

    _ -> error $ "observe: Model error. Only nodes can be observed, received: "
                                                                    ++ show t

  where unused = error "BUG: observe--not used"

initialize :: Term NodeIdx -> Double -> BayesianSimulator ()
initialize t d =
  case splitArray t of
    -- (TVar idx, its) | Just is <- mapM isConst its
    (TVar idx, []) ->
      do init <- getField cfgInitialize
         case IM.insertLookupWithKey unused idx d init of
           (Nothing, m1) -> upd (\s -> s { cfgInitialize = m1 })
           (Just _, _)   ->
              error $ "initialize: Model error. Node was initialized before: "
                                                                  ++ show t

    _ -> error $ "initialize: Model error. Only nodes can be initialized, received: "
                                                                    ++ show t

  where unused = error "BUG: initialize--not used"

monitor :: String -> Expr -> BayesianSimulator ()
monitor nm e = upd (\s -> s {  cfgMonitor = (nm, e) : cfgMonitor s })



