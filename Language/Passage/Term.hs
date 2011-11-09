{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Passage.Term where

import Control.Monad(mplus)
import qualified Data.IntSet as IS
import Data.Ratio(numerator,denominator)
import Data.Maybe(fromMaybe)

import Language.Passage.Utils
import qualified Language.Passage.Lang.LaTeX as LaTeX
import Language.Passage.Lang.LaTeX(LaTeX(..))


data Op = TLog | TNeg | TAdd | TMul | TSub | TDiv | TPow | TLogGamma | TExp
        | TCase -- for "arrays" of det. nodes.  1st arg index, rest array.
        | TIx
        | TLit String
        deriving (Eq, Show, Ord)

-- | A term in a stochastic context
data Term a = TVar a
            | TArr a      -- A node corresponding to an array
            | TConst Double
            | TApp Op [Term a]
          deriving (Eq, Ord, Show, Functor)

{-
sizeOf :: Term a -> Int
sizeOf (TVar{})    = 1
sizeOf (TConst{})  = 0
sizeOf (TApp _ xs) = 1 + sum (map sizeOf xs)
-}

type ArrVars = NodeIdx -> IS.IntSet

-- | Nodes hanging off of a term
leavesOfTerm :: ArrVars -> Term NodeIdx -> IS.IntSet
leavesOfTerm _   (TVar b)    = IS.singleton b
leavesOfTerm arr (TArr b)    = arr b
leavesOfTerm _   (TConst{})  = IS.empty
leavesOfTerm arr (TApp _ ts) = IS.unions (map (leavesOfTerm arr) ts)

-- | Returns 'True' for terms that are not applications.
isSimpleTerm :: Term a -> Bool
isSimpleTerm t =
  case t of
    TApp _ _ -> False
    TArr _   -> True
    TVar _   -> True
    TConst _ -> True

tcase :: Term a -> [Term a] -> Term a
tcase e es = TApp TCase (e:es)

tvar :: a -> Term a
tvar = TVar

tarr :: a -> Term a
tarr = TArr

tconst :: Double -> Term a
tconst = TConst

isConst :: Term a -> Maybe Double
isConst (TConst d)  = Just d
isConst _           = Nothing

un :: Op -> Term a -> Term a
un op x   = TApp op [x]

bin :: Op -> Term a -> Term a -> Term a
bin op x y = TApp op [x,y]


termOp :: Term a -> Maybe Op
termOp (TApp op _) = Just op
termOp _           = Nothing

logGamma :: Term a -> Term a
logGamma t = case t of
               TConst a -> TConst (lgg a)
               _        -> un TLogGamma t
 where lgg :: Double -> Double
       lgg z = 0.5 * (log (2*pi) - log z) + z*(log (z+1/(12*z-0.1/z)) - 1)



tIx :: Term a -> Term a -> Term a
tIx a i = TApp TIx [a,i]

-- Split a term into an arrya and indexes.
-- Example:  a[1][2][3]  --->  (a,[1,2,3])
splitArray :: Term a -> (Term a, [Term a])
splitArray t0 = loop t0 []
  where loop (TApp TIx [a,i]) is = loop a (i:is)
        loop t is                = (t,is)



--------------------------------------------------------------------------------
-- Smarter printing infrastructure

precedence :: Op -> (Fixity, Rational)
precedence op =
  case op of
    TExp    -> (Prefix, 100)
    TLog       -> (Prefix, 100)
    TLogGamma  -> (Prefix, 100)
    TCase      -> (Prefix, 100)
    TNeg    -> (Prefix, 100)
    TAdd    -> (Infix ToLeft,  6)
    TSub    -> (Infix ToLeft,  6)
    TMul    -> (Infix ToLeft,  7)
    TDiv    -> (Infix ToLeft,  7)
    TPow    -> (Infix ToRight, 8)
    TIx     -> (Infix ToLeft,  9)
    TLit s  -> (Prefix, 100)

instance PP Op where
  pp op =
    case op of
      TCase -> text "choose"
      TExp -> text "exp"
      TLogGamma -> text "logGamma"
      TLog -> text "log"
      TNeg -> text "-"
      TAdd -> text "+"
      TSub -> text "-"
      TMul -> text "*"
      TDiv -> text "/"
      TPow -> text "^"
      TIx  -> text "!"
      TLit s -> text s

wrapLatex :: Op -> Posn -> Maybe Op -> Doc -> Doc
wrapLatex _ _ Nothing doc = doc
wrapLatex op pos (Just op1) doc = if shouldWrap then wrap else doc
  where
  shouldWrap =
    case (pos,op) of
      (ToLeft,  TPow)  -> True
      (_, TPow)        -> False

      (_,  TAdd)       -> op1 == TIx
      (ToRight,  TIx)  -> op1 == TIx
      (_,        TIx)  -> False

      (ToLeft,  TSub)  -> False
      (_,       TSub)  -> op1 == TSub || op1 == TAdd || op1 == TNeg
                        || op1 == TIx

      (ToLeft,  TMul)  -> op1 == TSub || op1 == TAdd
      (_,       TMul)  -> op1 == TSub || op1 == TAdd || op1 == TNeg
                        || op1 == TIx

      (_,       TDiv)  -> False

      (_,       TLog)      -> op1 == TAdd || op1 == TSub || op == TMul
      (_,       TExp)      -> op1 == TAdd || op1 == TSub || op == TMul
      (_,       TLogGamma) -> op1 == TAdd || op1 == TSub || op == TMul
      (_,       TNeg )     -> op1 == TAdd || op1 == TSub || op == TMul
                            || op1 == TIx
      (_,       TCase)  -> False

  wrap = text "\\left(" <> doc <> text "\\right)"

instance LaTeX a => LaTeX (Term a) where

  latex (TApp op ts) =
    case op of
      TAdd -> dL <+> char '+' <+> dR
      TSub -> dL <+> char '-' <+> dR
      TMul -> dL <+> dR
      TDiv -> LaTeX.frac dL dR
      TPow -> LaTeX.pow dL dR
      TLog -> LaTeX.lg <+> dL
      TExp -> LaTeX.expon <+> dL
      TLogGamma -> LaTeX.logGamma dL
      TNeg -> char '-' <> dL
      TIx  -> dL <+> char '!' <+> dR  -- XXX: This could be nicer
      TCase -> let a : as = map latex ts
               in commaSep as <> char '_' <> braces a

      TLit s -> (LaTeX.literal s) dL
      where ds       = zipWith pr (ToLeft : ToRight : repeat None) ts
            pr pos t = wrapLatex op pos (termOp t) (latex t)
            dL : ds1 = ds
            dR : _   = ds1

  latex (TVar x)    = latex x
  latex (TArr x)    = latex x
  latex (TConst a)  = double a



ppTerm :: (PP a) => (Posn,Rational) -> Term a -> Doc
ppTerm (pos,prec) (TApp op [l,r])
  | (Infix dir, myprec) <- precedence op =
    let this = ppTerm (ToLeft,  myprec) l <+> pp op <+>
               ppTerm (ToRight, myprec) r
    in if myprec > prec || (myprec == prec && pos == dir)
          then this
          else parens this

ppTerm (_,p) (TApp op ts) =
  let this = pp op <+> commaSep [ ppTerm (None,0) t | t <- ts ]
  in if snd (precedence op) > p then this else parens this

ppTerm (_,n) (TArr x) = text "!" <> ppPrec n x
ppTerm (_,n) (TVar x) = text "?" <> ppPrec n x
ppTerm _ (TConst a)   = double a

instance PP a => PP (Term a) where
  ppPrec n = ppTerm (None,n)

liftTerm1 :: (Term a -> Term a) -> (Double -> Double) -> Term a -> Term a
liftTerm1 _ c (TConst a) = TConst (c a)
liftTerm1 s _ a          = s a

liftTerm2 :: (Term a -> Term a -> Term a) -> (Double -> Double -> Double) -> Term a -> Term a -> Term a
liftTerm2 _ c (TConst a) (TConst b) = TConst (a `c` b)
liftTerm2 s _ a          b          = a `s` b

tbd1 :: Show a => String -> Term a -> b
tbd1 w x = tbd ("Term." ++ w ++ " " ++ show x)

tbd2 :: Show a => String -> Term a -> Term a -> b
tbd2 w x y = tbd ("Term." ++ w ++ show (x, y))

instance (Eq a, Show a) => Num (Term a) where
  -- addition
  TConst 0 + b        = b
  a + TConst 0        = a
  TConst x + TConst y = TConst (x + y)
  a + (TApp TAdd [b,c]) = (a + b) + c
  a + TApp TNeg [b]   = a - b
  TApp TNeg [a] + b   = b - a
  TApp TDiv [a, x] + TApp TDiv [b, y]
    | x == y = (a+b) / x
  a + b = liftTerm2 (bin TAdd) (+) a b
{-
  a + b
    | sizeOf a < sizeOf b = walkAdd a b
    | True                = walkAdd b a
-}

  -- multiplication
  TConst 0 * _        = TConst 0
  TConst 1 * b        = b
  TConst (-1) * b     = negate b
  TConst x * TConst y = TConst (x * y)
  TConst x * TApp TDiv [TConst y, z]
                      = TConst (x*y) / z
  TConst x * TApp TDiv [z, TConst y]
                      = TConst (x/y) * z
  a * b@(TConst _)    = b * a             -- constants float left
  TApp TNeg [a] * b   = negate (a * b)
  a * TApp TNeg [b]   = negate (a * b)
  a * b               = liftTerm2 (bin TMul) (*) a b

  -- subtraction
  a - TConst 0      = a
  a - TApp TNeg [b] = a + b
  TApp TNeg [a] - b = negate (a + b)
  TApp TSub [b, c] - d = TApp TSub [b, c+d]
  a - b             = liftTerm2 (bin TSub) (-) a b

  -- negation
  negate (TApp TNeg [x]) = x
  negate x               = liftTerm1 (un TNeg) negate x

  -- others
  abs         = liftTerm1 (tbd1 "abs")    abs
  signum      = liftTerm1 (tbd1 "signum") signum
  fromInteger = TConst . fromInteger

instance (Eq a, Show a) => Fractional (Term a) where
  -- division
  a                          / TConst 1          = a
  TConst x                   / TConst y | y /= 0 = TConst (x / y)
  (TApp TDiv [TConst c1, x]) / TConst c2         = TConst (c1/c2) / x
  a                          / b                 = liftTerm2 (bin TDiv) (/) a b

  recip x                      = 1 / x
  fromRational x               = fromInteger (numerator x) / fromInteger (denominator x)

instance (Eq a, Show a) => Floating (Term a) where
  pi      = TConst pi
  exp     = liftTerm1 (un TExp)     exp
  sqrt    = liftTerm1 (tbd1 "sqrt")    sqrt
  log     = liftTerm1 (un TLog)        log

  -- power
  _ ** TConst 0 = TConst 1
  TConst 0 ** _ = TConst 0
  a ** b        = liftTerm2 (bin TPow) (**) a b

  -- TBD: Add support for these as needed
  logBase = liftTerm2 (tbd2 "logBase") logBase
  sin     = liftTerm1 (tbd1 "sin")     sin
  tan     = liftTerm1 (tbd1 "tan")     tan
  cos     = liftTerm1 (tbd1 "cos")     cos
  asin    = liftTerm1 (tbd1 "asin")    asin
  atan    = liftTerm1 (tbd1 "atan")    atan
  acos    = liftTerm1 (tbd1 "acos")    acos
  sinh    = liftTerm1 (tbd1 "sinh")    sinh
  tanh    = liftTerm1 (tbd1 "tanh")    tanh
  cosh    = liftTerm1 (tbd1 "cosh")    cosh
  asinh   = liftTerm1 (tbd1 "asinh")   asinh
  atanh   = liftTerm1 (tbd1 "atanh")   atanh
  acosh   = liftTerm1 (tbd1 "acosh")   acosh

-- Add two terms symbolically, trying to simplify as much as possible.
-- TODO: This process is currently quite ad-hoc, due to the lack of a
-- well defined normal form for terms. Also, developers will likely to
-- be able to add their own rules if necessary..
--
-- sAdd returns Nothing if it did Nothing, and Just t, if it was able
-- to do something interesting.
sAdd :: (Show t, Eq t) => Term t -> Term t -> Maybe (Term t)

-- x + x == 2x
sAdd x y
  | x == y    = Just (2 * x)

-- x - x == 0
sAdd x y
  | x == -y || -x == y = Just 0

-- a + ab = a (b+1)
-- b + ab = b (a+1)
sAdd x (TApp TMul [a,b])
  | x == a    = Just (a * (b + 1))
  | x == b    = Just (b * (a + 1))

-- x + (q + x)  = q + 2*x
sAdd x (TApp TAdd [q, y])
  | x == y    = Just (q + 2*x)
  | x == q    = Just (y + 2*x)

-- -x + (q - x) = q - 2*x
sAdd (TApp TNeg [x]) (TApp TSub [q, y])
  | x == y    = Just (q - 2*x)

-- ay + ab = a (y+b)
-- xb + ab = (x+a) b
sAdd (TApp TMul [x,y]) (TApp TMul [a,b])
  | x == a    = Just (x * (y `add` b))
  | y == b    = Just ((x `add` a) * y)
  where add p q = fromMaybe (p + q) (sAdd p q)

-- Try associative rule to see if it simplifies things
-- We arbitrarily prefer the first rule below if both apply
-- x + (a+b) = (x+a) + b
-- x + (a+b) = a + (x+b)
sAdd x (TApp TAdd [a,b])  = case fmap (+ b) (sAdd x a) `mplus` fmap (a +) (sAdd x b) of
                              Nothing -> Nothing
                              r@(Just (TApp TAdd [t1, t2])) -> maybe r Just (sAdd t1 t2)
                              r       -> r

-- x + (a - b) = (x+a) - b
sAdd x (TApp TSub [a,b])  = fmap (subtract b) (sAdd x a)

-- Otherwise we weren't able to do anything smart; so just report Nothing
sAdd _ _ = Nothing


-- Walk a deep-tree and add a term; taking care of collapsing if possible.
walkAdd :: (Eq a, Show a) => Term a -> Term a -> Term a
walkAdd a b = case walk b of
                Just b' -> b'
                Nothing -> bin TAdd a b
 where walk t | a == t = Just (2*t)
       walk (TApp TMul [c, t]) | a == t = Just $ (c+1) * t
       walk t          = case t of
                           TApp TAdd [x, y] -> case walk x of
                                                 Just x' -> Just $ bin TAdd x' y
                                                 Nothing -> case walk y of
                                                              Just y' -> Just $ bin TAdd x y'
                                                              Nothing -> Nothing
                           _ -> Nothing

-- Split a term into its summands.
-- We use distributivity laws to try to get as smaller terms as possible,
-- in the hope that they might contain fewer varaibles.
-- (i.e., we convert the term to a sum-of-products).
summands :: (Eq a, Show a) => Term a -> [Term a]
summands te = loop [te]
  where loop [] = []
        loop (t : ts) =
          case t of
            TApp op [x,y]
              -- x+y; split them
              | op == TAdd          -> loop (x : y : ts)
              -- x-y; split them
              | op == TSub          -> loop (x : negate y : ts)
              -- x*(y1+y2+..+yN); distribute
              | op == TMul, composite ys -> loop (map (x *) ys ++ ts)
              -- (x1+x2+..+xN)*y; distribute
              | op == TMul, composite xs -> loop (map (* y) xs ++ ts)
              -- (x1+x2+..+xN)/y; distribute
              | op == TDiv, composite xs -> loop (map (/ y) xs ++ ts)
              where xs = summands x
                    ys = summands y
                    -- Does it have at least two terms?
                    composite (_ : _ : _) = True
                    composite _           = False
            -- Otherwise keep t; and factor the rest
            _                 -> t : loop ts


-- Split a term into a product of two terms, with the property that
-- only the first term contains the given variable.
factorVar :: ArrVars -> NodeIdx -> Term NodeIdx -> (Term NodeIdx, Term NodeIdx)
factorVar arr x t =
  case t of
    TApp op ts
      | op == TNeg, [t1] <- ts  -> let (a,b) = factorVar arr x t1 in (a, negate b)
      | op == TMul -> let ([a,b],bs) = unzip $ map (factorVar arr x) ts
                      in (opt_mul a b, product bs)
      | op == TDiv -> let ([a,b],[c,d]) = unzip $ map (factorVar arr x) ts
                      in (a / b, c / d)
    _ | x `IS.member` leavesOfTerm arr t -> (t, 1)
    _ -> (1,t)

  where

  opt_mul a b = fromMaybe (a * b) (mul a b)

  mul a b | a == b                                    = Just (a ** 2)
  mul (TApp TPow [a,b]) (TApp TPow [a1, c]) | a == a1 = Just (a ** (b + c))
  mul a (TApp TPow [b, c]) | a == b                   = Just (a ** (1 + c))
  mul (TApp TPow [b,c]) a | a == b                    = Just (a ** (1 + c))
  mul a (TApp TMul [b,c]) =
    case mul a b of
      Just b1 -> Just (opt_mul b1 c)
      Nothing -> case mul a c of
                   Just c1 -> Just (opt_mul b c1)
                   Nothing -> Nothing
  mul a (TApp TDiv [b,c]) = Just (opt_mul a b / c)
  mul _ _ = Nothing
