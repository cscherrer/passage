-- | A simple graph coloring algorithm.
module Language.Passage.GraphColor (groupByColor) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (sortBy, groupBy)
import Data.Foldable(foldl')
import Data.Maybe (mapMaybe)
import Data.Function (on)

type Color    = Int
type Coloring a = IM.IntMap (a,Color)

choose :: Coloring a -> (Int, (a, IS.IntSet)) -> Coloring a
choose coloring (key,(a,ns)) = IM.insert key (a, head candidates) coloring
  where used       = map snd $ mapMaybe (`IM.lookup` coloring ) $ IS.toList ns
        candidates = [ x | x <- [ 0 .. ], not (x `elem` used) ]

addNode :: IM.IntMap (a,IS.IntSet)
        -> (Int, a, IS.IntSet)
        -> IM.IntMap (a, IS.IntSet)
addNode g0 (x,a,xs) = IS.fold addBack (IM.insertWith jnUseNew x node g0) xs
  where
  addBack y g     = IM.insertWith jnUseOld y (missing y,IS.singleton x) g
  node            = (a,xs)
  jnUseOld (_,vs1) (b,vs2)  = (b, IS.union vs1 vs2)
  jnUseNew (_,vs1) (_,vs2)  = (a, IS.union vs1 vs2)
  missing y = error ("BUG: Variable " ++ show y ++ " is missing?")

colorG :: [(Int,a,IS.IntSet)] -> Coloring a
colorG = foldl' choose IM.empty
       . sortBy earlier
       . IM.toList
       . foldl' addNode IM.empty

  where earlier (_,(_,x)) (_,(_,y)) = compare (IS.size y) (IS.size x)


colorGroups :: Coloring a ->  [[(Int,a)]]
colorGroups = map (map snd)
             . groupBy ((==) `on` fst)
             . sortBy (compare `on` fst)
             . map swap
             . IM.toList

  where swap (x,(a,c)) = (c,(x,a))


groupByColor :: [(Int,a, IS.IntSet)] -> [[(Int,a)]]
groupByColor = colorGroups . colorG



