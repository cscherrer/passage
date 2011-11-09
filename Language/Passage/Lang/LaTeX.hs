module Language.Passage.Lang.LaTeX where

import Language.Passage.Utils
import qualified Data.Set as S


class LaTeX t where
  latex :: t -> Doc

instance LaTeX Double where
  latex = double

instance LaTeX Int where
  latex = int

cmd :: String -> [Doc] -> Doc
cmd a as = char '\\' <> text a <> case as of
                                    [] -> empty
                                    _  -> hcat (map braces as)

frac, pow :: Doc -> Doc -> Doc
frac a b  = cmd "frac" [ a, b ]
pow a b   = braces a <> char '^' <> braces b

lg  :: Doc
lg = cmd "log" []

expon = cmd "exp" []

logGamma :: Doc -> Doc
logGamma a = cmd "log" [cmd "Gamma" [a]]

literal :: String -> Doc -> Doc
literal s a = cmd s [a]

knownVars :: S.Set String
knownVars = S.fromList
  [ "alpha", "beta", "gamma", "delta", "epsilon", "varepsilon"
  , "zeta", "eta", "theta", "vartheta", "kappa", "lambda", "mu", "nu", "xi"
  , "pi", "varpi", "rho", "varrho", "sigma", "varsigma"
  , "tau", "upsilon", "phi", "varphi", "chi", "psi", "imega"
  ]

var :: String -> Doc
var x | x `S.member` knownVars = cmd x []
      | short x                = text x
      | True                   = cmd "ensuremath" [ cmd "mathit" [ text x ]]
  where short [_] = True
        short _   = False

math :: Doc -> Doc
math x  = char '$' <> x <> char '$'

env :: String -> [Doc] -> Doc -> Doc
env x opts body = cmd "begin" (text x : opts) $$ body $$ cmd "end" [text x]

sim :: Doc
sim = cmd "sim" []

row :: [Doc] -> Doc
row xs = hsep (punctuate (text "&") xs) <+> text "\\\\"

hline :: Doc
hline = cmd "hline" []

mathcal :: Doc -> Doc
mathcal d = cmd "mathcal" [d]

mathrm :: Doc -> Doc
mathrm d = cmd "mathrm" [d]
