module Language.Passage.SliceSampleMWC where

import System.Random.MWC
import Control.Monad.Primitive

{- GNUPLOT commands to see the distribution:
binwidth=5
bin(x,width)=width*floor(x/width)

plot 'datafile' using (bin($1,binwidth)):(1.0) smooth freq with boxes
-}

{-
Some R code for testing:

db <- read.table('data.beta', header=F)[,1]
dg <- read.table('data.gamma', header=F)[,1]
dn <- read.table('data.norm', header=F)[,1]

library(MASS)

s <- seq(0,5,length=1000)

truehist(db)
lines(s, dbeta(s,2,5),col='red')

truehist(dg)
lines(s, dgamma(s,2,5),col='red')

qqnorm(dn)

-}

genAll :: IO ()
genAll = mapM_ (genTest 100000) [0..3]

genTest :: Integer -> Int -> IO ()
genTest cnt i =  do
  putStr $ "Generating " ++ show nm ++ ". "
  xs <- genPts cnt slicer 1 5 source
  writeFile nm $ unlines (map show xs)
  putStrLn "Done."
 where gammaLL a b x =   (a - 1) * log x - b * x   
       normLL = (* 0.5) . negate . (**2)
       betaLL a b x = (a - 1) * log x + (b-1) * log (1 - x)
       (nm, slicer, source) = 
        [ ("data.gamma", slicePos, gammaLL 2 5)
        , ("data.norm", slice, normLL)
        , ("data.beta", sliceUnit, betaLL 2 5)
        , ("data.truncNormal",sliceUnit, normLL)
        ] !! i
type Slicer = Gen RealWorld
     -> Double
     -> Double
     -> (Double -> Double)
     -> IO (Double, Double)

genPts :: Integer -> Slicer -> Double -> Double -> (Double -> Double) -> IO [Double]
genPts cnt slicer w initX ll = do 
  g <- create
  let 
    go _  0 = return []
    go x0 c = do 
      (x1,_) <- slicer g w x0 ll
      xs <- go x1 (c-1)
      return (x1:xs) 
  go initX cnt

sliceUnit :: Slicer
sliceUnit g w x0 ll = genericSlice g w x0 ll (\_ _ -> 0) (\_ _ -> 1)

slicePos :: Slicer
slicePos g w x0 ll = genericSlice g w x0 ll (\_ _ -> 0) (\y lo -> search y (right_pts y lo))
  where 
  search y  = head . dropWhile (\p -> ll p > y)
  right_pts _ lo = [ right, right + w .. ]
          where right = x0 + lo

slice :: Slicer
slice g w x0 ll = genericSlice g w x0 ll (\y lo -> search y (left_pts y lo)) (\y lo -> search y (right_pts y lo))
  where search y  = head . dropWhile (\p -> ll p > y)
        left_pts _ lo = [ left, left - w .. ]
          where left = x0 - lo
        right_pts _ lo = [ right, right + w .. ]
           where right = x0 - lo + w

genericSlice :: (PrimMonad m) =>
     Gen (PrimState m)
     -> Double
     -> Double
     -> (Double -> Double)
     -> (Double -> Double -> Double)
     -> (Double -> Double -> Double)
     -> m (Double, Double)
genericSlice g w x0 ll left right = do
  r <- uniform g
  let y = log r + ll x0
  lo <- uniformR (0,w) g
  pickRandom g x0 y ll (left y lo) (right y lo)


pickRandom
  :: (Control.Monad.Primitive.PrimMonad m) =>
     Gen (Control.Monad.Primitive.PrimState m)
     -> Double
     -> Double
     -> (Double -> Double)
     -> Double
     -> Double
     -> m (Double, Double)

pickRandom g x0 y ll = go
  where 
  go l r = do
    x1 <- uniformR (l, r) g
    let ll_x1 = ll x1
    if ll_x1 < y then
      if x1 < x0 then go x1 r else go l x1
                 else return (x1, ll_x1)
