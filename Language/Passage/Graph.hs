{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Passage.Graph where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS
import Data.List(foldl')

-- import Debug.Trace

import Language.Passage.AST
import Language.Passage.Term
import Language.Passage.Utils
import Language.Passage.Lang.LaTeX(LaTeX(..))
import qualified Language.Passage.Lang.LaTeX as LaTeX


stoPostLL :: StoVar -> Term NodeIdx
stoPostLL sv = sum [ b * a | (a,b) <- M.toList (stoPostDistLL sv) ]

emptyBayesianGraph :: BayesianGraph
emptyBayesianGraph = BayesianGraph { stoNodes = IM.empty
                                   , stoArryas = IM.empty
                                   }

addToStoLL  :: NodeIdx -> Term NodeIdx -> BayesianGraph -> BayesianGraph
addToStoLL ix t bg = bg { stoNodes = IM.alter addLL ix (stoNodes bg) }
  where
  (x,c)    = factorVar (fvsArray bg) ix t
  -- addLL sv = sv { stoPostDistLL = M.insertWith plus x c (stoPostDistLL sv) }
  addLL (Just sv) = Just $! sv { stoPostDistLL = M.insertWith' plus x c (stoPostDistLL sv) }

  addLL Nothing = Nothing

  plus :: (PP a, Eq a, Show a) => Term a -> Term a -> Term a
  plus a b = {- trace ("plus: " ++ "\n   a: " ++ show (pp a)
                             ++ "\n   a: " ++ show a
                             ++ "\n   b: " ++ show (pp b)
                             ++ "\n   b: " ++ show b
                             ++ "\n a+b: " ++ show (pp result)) -}
             result
    where result = maybe (a+b) id (sAdd a b)
--------------------------------------------------------------------------------

buildBayesianGraph :: BayesianNetwork a -> (a, BayesianGraph)
buildBayesianGraph nw = (a, computeLL g)
  where (a, g) = extractNetwork nw

-- | Compute the log-likelihood for a stochastic variable.
computeLL :: BayesianGraph -> BayesianGraph
computeLL bg = foldl' addDef bg (IM.elems (stoNodes bg))
  where addDef m sv   = foldl' addSum m (summands (priLL (stoVarPrior sv)))
        addSum m t    = IS.fold (\i m1 -> addToStoLL i t m1) m
                                            (leavesOfTerm (fvsArray bg) t)




--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

data PPVar = PPName String
           | PPArr String [Int]
            deriving Show

nameToPPName :: BayesianGraph -> StoVar -> PPVar
nameToPPName bg sv =
  case stoVarName sv of
    Unnamed y -> PPName ("v" ++ show y)
    Named y ->   PPName y
    InArray a b ->
      case IM.lookup a (stoArryas bg) of
        Just ai -> PPArr (arrayName ai) b
        Nothing -> PPArr ("bug_unknown_array_" ++ show a) b

varName :: BayesianGraph -> NodeIdx -> PPVar
varName bg x = case IM.lookup x (stoNodes bg) of
                 Just sv -> nameToPPName bg sv
                 Nothing ->
                   case IM.lookup x (stoArryas bg) of
                     Just ai -> PPName (arrayName ai)
                     Nothing -> PPName ("bug_unknown_variable_" ++ show x)

namedTerm :: BayesianGraph -> Term NodeIdx -> Term PPVar
namedTerm bg = fmap (varName bg)

instance PP PPVar where
  pp (PPName x)   = text x
  pp (PPArr x ys) = text x <> hcat (map (brackets . int) ys)

instance LaTeX PPVar where
  latex (PPName x)   = LaTeX.var x
  latex (PPArr x ys) = LaTeX.var x <> char '_' <>
                            braces (hcat (punctuate comma (map int ys)))


instance PP BayesianGraph where
  pp bg = vcat (map ppSto (IM.elems (stoNodes bg)))

    where
    ppT t     = pp (namedTerm bg t)
    ppSto sv  = pp (nameToPPName bg sv) <+> text "~~" <+>
                ppPri (stoVarPrior sv) <+>
                text ":"  <+> ppT (stoPostLL sv)

    ppPri i = text (priName i) <+>
                commaSep (map (pp . namedTerm bg) (priParams i))

instance LaTeX BayesianGraph where
  latex bg =
    LaTeX.env "tabular" [text "l"] $ vcat $ map (\x -> LaTeX.row [x])
      [ LaTeX.env "tabular" [text "l l"]
            (LaTeX.row [ text "Prior distribution"
                 , text  "Posterior log-likelihood" ] $$
          vcat (map ppSto (IM.elems (stoNodes bg))))
      ]

    where ppT t        = latex (namedTerm bg t)
          row x y z    = LaTeX.row (map LaTeX.math [ x <+> LaTeX.sim <+> y, z])
          ppSto sv =
            row (latex (nameToPPName bg sv))
                (ppPri (stoVarPrior sv))
                (ppT (stoPostLL sv))

          ppPri i = latexDist (priName i) (map (namedTerm bg) (priParams i))


