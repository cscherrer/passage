{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE PatternGuards              #-}

module Language.Passage.AST where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Array as A
import MonadLib
import Data.Char(isUpper)


import Language.Passage.Utils
import Language.Passage.Lang.LaTeX(LaTeX(..))
import qualified Language.Passage.Lang.LaTeX as LaTeX
import Language.Passage.Term

------------------------------------------------------------------------
-- * AST Nodes in a Bayesian model description
------------------------------------------------------------------------

type Expr = Term NodeIdx

-- | A Bayesian Network is a collection of stochastic nodes.
-- Stochastic nodes may be (optionally) grouped into arrays.
data BayesianGraph = BayesianGraph
  { stoNodes      :: !(IM.IntMap StoVar)
  , stoArryas     :: !(IM.IntMap ArrayInfo)
  } deriving Show

-- | A Stochastic variable.
data StoVar = StoVar
  { stoVarName    :: StoVarName
  , stoVarPrior   :: PriorInfo
  , stoPostDistLL :: !(M.Map Expr Expr)
    -- ^ Maps terms that mention the variable to their coefficients,
    -- which do not depend on the variable.  The term for the
    -- distribution is the sum of the products of the map elements
    -- (see 'stoPostLL').
  } deriving Show

-- | The name of a stochastic variable.
data StoVarName
  = Unnamed !NodeIdx        -- ^ Anonymous stand-alone variable
  | InArray !NodeIdx ![Int] -- ^ This sto var belongs to an array
  | Named !String           -- ^ Standalone variable, with a user-name
    deriving Show

-- | Information about the prior distribution of a stochastic variable.
data PriorInfo = PriorInfo
  { priName     :: String
  , priParams   :: [Term NodeIdx]
  , priSupport  :: DistSupport NodeIdx
  , priLL       :: Term NodeIdx
  } deriving Show


-- | The description of an atomic distribution.
data Distribution = Distribution
  { distName    :: String
  , distParams  :: [Expr]
  , distSupport :: DistSupport NodeIdx
  , distLL      :: Expr -> Expr
  }


-- | Support of a distribution
data DistSupport a
  = Real
  | Discrete (Maybe (Term a))     -- upper bound, from 0 to this number
  | PosReal
  | Interval (Term a) (Term a)    -- lower/upper
    deriving (Show,Functor)


-- | Information about an array (vecotr/matrix) of stochastic variables.
data ArrayInfo = ArrayInfo
  { arrayName       :: String
  , arrayDimensions :: [(Int,Int)]
  , arrayVars       :: IS.IntSet
  } deriving Show


exprToVar :: Expr -> NodeIdx
exprToVar (TVar x) = x
exprToVar e        = error $ "Expected a variable expression, got: " ++
                              (show (pp e))

fvsSupport :: ArrVars -> DistSupport NodeIdx -> IS.IntSet
fvsSupport arr sup =
  case sup of
    Real          -> IS.empty
    Discrete t    -> maybe IS.empty (leavesOfTerm arr) t
    PosReal       -> IS.empty
    Interval a b  -> IS.union (leavesOfTerm arr a) (leavesOfTerm arr b)


fvsArray :: BayesianGraph -> NodeIdx -> IS.IntSet
fvsArray bg ix = case IM.lookup ix (stoArryas bg) of
                   Just ai -> arrayVars ai
                   Nothing -> IS.empty    -- XXX: report error?



latexDist :: LaTeX a => String -> [Term a] -> Doc
latexDist name params = fun <+> commaSep (map latex params)
  where fun = case name of
                [n] | isUpper n -> LaTeX.mathcal (char n)
                _               -> LaTeX.mathrm (text name)


------------------------------------------------------------------------
-- * Constructing the program
------------------------------------------------------------------------
data ASTState = ASTState
  { curIdx          :: !Int
  , declaredArrays  :: !(IM.IntMap ArrayInfo)
  , generatedNodes  :: !(IM.IntMap StoVar)
  }

newtype BayesianNetwork a = BayesianNetwork (StateT ASTState Id a)
                          deriving (Functor, Applicative, Monad)

updateState :: (ASTState -> (a,ASTState)) -> BayesianNetwork a
updateState f = BayesianNetwork (sets f)

updateState_ :: (ASTState -> ASTState) -> BayesianNetwork ()
updateState_ f = updateState (\s -> let s1 = f s in seq s1 ((), s1))

getState :: BayesianNetwork ASTState
getState = BayesianNetwork get


using :: Distribution -> BayesianNetwork Expr
using d = updateState $ \st ->
  let i  = curIdx st
  in ( tvar i
     , st { curIdx = i + 1
          , generatedNodes = IM.insert i (toStoVar i d) (generatedNodes st)
          }
     )

toStoVar :: NodeIdx -> Distribution -> StoVar
toStoVar i d = StoVar
  { stoVarName  = Unnamed i
  , stoVarPrior = PriorInfo
      { priName     = distName d
      , priParams   = distParams d
      , priSupport  = distSupport d
      , priLL       = distLL d (tvar i)
      }
  , stoPostDistLL   = M.empty
  }

-- NOTE: The posterior LLs are not yet computed in the graph that's returned.
extractNetwork :: BayesianNetwork a -> (a, BayesianGraph)
extractNetwork (BayesianNetwork m) =
  ( a
  , BayesianGraph { stoNodes  = generatedNodes s
                  , stoArryas = declaredArrays s
                  }
  )
  where (a, s) = runId (runStateT start m)
        start  = ASTState { curIdx = 0
                          , declaredArrays = IM.empty
                          , generatedNodes = IM.empty
                          }

type Vector    = [Expr] -> Expr
type Matrix    = [Expr] -> Expr
type NodeArray = [Expr] -> Expr

-- | Create a 1D vector
vector :: (Int,Int)                      -- ^ Bounds for the vector indexes
       -> (Int -> BayesianNetwork Expr)  -- ^ Initializer (should return a node)
       -> BayesianNetwork ([Expr] -> Expr)
vector b i = nodeArray [b] (i . head)

-- | Create a 2D matrix
matrix :: (Int,Int) -> (Int,Int)          -- ^ Bounds for 1st and 2nd dimensions.
       -> ([Int] -> BayesianNetwork Expr) -- ^ Initializer
       -> BayesianNetwork ([Expr] -> Expr)
matrix b1 b2 = nodeArray [b1, b2]

-- | Create an >= 3D array
nodeArray :: [(Int,Int)]                     -- ^ Bounds for each dimension.
       -> ([Int] -> BayesianNetwork Expr) -- ^ Initializer
       -> BayesianNetwork ([Expr] -> Expr)
nodeArray bds initializer =
  do (ix,ai,mp) <- newArray bds initializer
     return (lkpArrayMap ix ai mp)



data ArrayMap = A !(A.Array Int ArrayMap)
              | V !Expr

lkpArrayMap :: NodeIdx -> ArrayInfo -> ArrayMap -> [Expr] -> Expr
lkpArrayMap x0 _ am = loop (tarr x0) am
  where
  loop _ (V x) []     = x
  loop e (A _) []     = e
  loop e (A a) (i : is)
    | Just j <- toIx i  = loop (tIx e i) (a A.! j) is
  loop e _ is           = foldl tIx e is  -- XXX: could do some more checking!

  toIx (TConst d) = Just (floor d) -- XXX: it'd be better to use proper types...
  toIx _          = Nothing


newArray :: [(Int,Int)]
         -> ([Int] -> BayesianNetwork Expr)
         -> BayesianNetwork (NodeIdx, ArrayInfo, ArrayMap)
newArray bds0 initializer =
  do aix <- updateState $ \st -> let i = curIdx st in (i, st { curIdx = 1 + i })
     let bds = map dimOK bds0
     (vars,m) <- loop aix IS.empty bds []
     let ai = ArrayInfo { arrayName       = "a" ++ show aix
                        , arrayDimensions = bds
                        , arrayVars       = vars
                        }

     updateState $ \s ->
        ( (aix, ai, m)
        , s { declaredArrays = IM.insert aix ai (declaredArrays s) }
        )

  where
  dimOK d@(x,y) | x <= y = d
  dimOK d = longError [ "Invalid array bounds:"
                      , "   *** Bounds: " ++ show d
                      ]

  -- The state of "loop"
  --  aix:  RO, index of the array that we are initializing
  --  vars: RW, a set of all the variables in the array (to be stored for later)
  --  bds:  RW, remaining array dimnesions to process
  --  ixes: RW, (reversed) path of indexes to current elent which to initialize

  -- We pass vars explicitly, rather then putting it in the state to
  -- avoid constant updates to the array map.
  loop aix vars [] ixes0 =
    do let ixes = reverse ixes0
       e <- initializer ixes
       let v = exprToVar e
       updateState $ \st ->
         let vars1 = IS.insert v vars
         in vars1 `seq`
         ( (vars1, V e)
         , let upd sv =
                 case stoVarName sv of
                   Unnamed _ -> sv { stoVarName = InArray aix ixes }
                   InArray a' ixes' -> longError
                     [ "Variable already belongs to an array:"
                     , "  *** Array: "   ++ lkpArrayName st a'
                     , "  *** Location: " ++ show ixes'
                     ]
                   Named s   -> longError
                     [ "Cannot add explicitly named variables to an array:"
                     , "  *** Variable: " ++ s
                     ]
           in st { generatedNodes = IM.adjust upd v (generatedNodes st) }
          )

  loop aix vars0 (bds@(from,to) : bdss) ixes =

    let loop1 vars as i | i <= to =
          do (vars1,a)  <- loop aix vars bdss (i:ixes)
             loop1 vars1 (a:as) (i+1)
        loop1 vars as _ = return (vars, A $ A.array bds
                                          $ zip [ from .. to ] (reverse as))
    in loop1 vars0 [] from







longError :: [String] -> a
longError = error . unlines


infixl 1 //

-- This operator is used to provide a custom name for a given variable.
(//) :: BayesianNetwork Expr -> String -> BayesianNetwork Expr
m // x =
  do e <- m
     let v = exprToVar e
     updateState $ \s -> (e, newState s v)

  where
  newState s v =
    case IM.lookup v (generatedNodes s) of
      -- XXX: This looks up the name twice.
      Just sv -> s { generatedNodes = IM.insert v (setName s sv)
                                                         (generatedNodes s) }
      Nothing ->
        case IM.lookup v (declaredArrays s) of
          Just ai -> s { declaredArrays =
                                    IM.insert v ai { arrayName = x }
                                                        (declaredArrays s) }
          Nothing -> longError
            [ "Attempt to rename an unknown node:"
            , "  *** Node: " ++ show v
            ]

  setName s sv = case stoVarName sv of
                   Unnamed _ -> sv { stoVarName = Named x }
                   Named n   -> longError
                      [ "Cannot rename a variable multiple times: "
                      , "  *** old name: " ++ n
                      , "  *** new name: " ++ x
                      ]
                   InArray a is -> longError
                      [ "Cannot rename array vairable: "
                      , "  *** array: " ++ lkpArrayName s a ++ show is
                      , "  *** new name" ++ x
                      ]


lkpArrayName :: ASTState -> NodeIdx -> String
lkpArrayName st a = case IM.lookup a (declaredArrays st) of
                      Nothing -> "(unknown?)"
                      Just ai -> arrayName ai


{-

Do we need to support arrays of deterministic variables?
Example:

do x <- bernoulli 0.5
   a <- detVector (1,100) $ \i -> 2 * i
   return (a ! x)

This requires prpoer support for determinsitc nodes,
which is broken at present.

-}


