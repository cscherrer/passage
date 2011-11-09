module Language.Passage.Utils
  ( module Language.Passage.Utils
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint hiding (float,double,rational)
import Data.Ratio(numerator,denominator)
import Data.Word

-- | A node index
type NodeIdx = Int

tbd :: String -> a
tbd msg = error $ "TBD: " ++ msg



data Fixity = Prefix | Infix Posn
data Posn   = ToLeft | ToRight | None
              deriving Eq


-- | Pretty printing
class Show t => PP t where
  ppPrec :: Rational ->  t -> Doc
  pp     :: t -> Doc

  pp        = ppPrec 0
  ppPrec _  = pp

instance PP Double where
  pp = double

instance PP Int where
  pp = int

instance PP Word where
  pp = text . show

instance PP Integer where
  pp = integer

instance PP Char where
  pp = char

commaSep :: [Doc] -> Doc
commaSep = parens . hsep . punctuate comma


ppFrac :: Show a => a -> Doc
ppFrac x = case break (== '.') candidate of
             (as,_:bs) | all (== '0') bs  -> text as
             _ -> text candidate
  where candidate = show x

float :: Float -> Doc
float = ppFrac

double :: Double -> Doc
double = ppFrac

rational :: Rational -> Doc
rational r = integer (numerator r) <> text "/" <> integer (denominator r)
