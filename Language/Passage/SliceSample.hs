module Language.Passage.SliceSample where

import System.Random

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

genPts :: Integer -> (StdGen -> Double -> Double -> (Double -> Double) -> (Double, Double, StdGen))
        -> Double -> Double -> (Double -> Double) -> IO [Double]
genPts cnt slicer w initX ll = do
    g <- newStdGen
    return $ go g initX cnt
  where go _  _  0 = []
        go g0 x0 c = let (x1, _, g1) = slicer g0 w x0 ll
                     in x1 : go g1 x1 (c-1)

sliceUnit :: StdGen                   -- ^ Source of randomness
          -> Double                   -- ^ Width of initial sampling interval
          -> Double                   -- ^ variable
          -> (Double -> Double)       -- ^ Log likelyhood, in terms of varibale
          -> (Double, Double, StdGen) -- ^ New value for variable, together with its log-likelihood
sliceUnit g w x0 ll = genericSlice g w x0 ll (\_ _ -> 0) (\_ _ -> 1)

slicePos :: StdGen                   -- ^ Source of randomness
         -> Double                   -- ^ Width of initial sampling interval
         -> Double                   -- ^ variable
         -> (Double -> Double)       -- ^ Log likelyhood, in terms of varibale
         -> (Double, Double, StdGen) -- ^ New value for variable, together with its log-likelihood
slicePos g w x0 ll = genericSlice g w x0 ll (\_ _ -> 0) (\y lo -> search y (right_pts y lo))
  where search y  = head . dropWhile (\p -> ll p > y)
        right_pts _ lo = [ right, right + w .. ]
          where right = x0 + lo

slice :: StdGen                   -- ^ Source of randomness
      -> Double                   -- ^ Width of initial sampling interval
      -> Double                   -- ^ variable
      -> (Double -> Double)       -- ^ Log likelyhood, in terms of varibale
      -> (Double, Double, StdGen) -- ^ New value for variable, together with its log-likelihood
slice g w x0 ll = genericSlice g w x0 ll (\y lo -> search y (left_pts y lo)) (\y lo -> search y (right_pts y lo))
  where search y  = head . dropWhile (\p -> ll p > y)
        left_pts _ lo = [ left, left - w .. ]
          where left = x0 - lo
        right_pts _ lo = [ right, right + w .. ]
           where right = x0 - lo + w

genericSlice :: StdGen                        -- ^ Source of randomness
             -> Double                        -- ^ Width of initial sampling interval
             -> Double                        -- ^ variable
             -> (Double -> Double)            -- ^ Log likelyhood, in terms of varibale
             -> (Double -> Double -> Double)  -- ^ left bound
             -> (Double -> Double -> Double)  -- ^ right bound
             -> (Double, Double, StdGen)      -- ^ New value for variable, together with its log-likelihood
genericSlice g w x0 ll left right =
  let (r, g1)    = randomR (0, 1) g
      y         = log r + ll x0
      (lo, g2)  = randomR (0, w) g1
   in pickRandom g2 x0 y ll (left y lo) (right y lo)


pickRandom :: StdGen -> Double -> Double -> (Double -> Double) -> Double -> Double -> (Double, Double, StdGen)
pickRandom g0 x0 y ll = go g0
  where go g l r = if ll_x1 < y then if x1 < x0 then go g1 x1 r
                                                else go g1 l x1
                                else (x1, ll_x1, g1)
         where (x1, g1) = randomR (l, r) g
               ll_x1   = ll x1
