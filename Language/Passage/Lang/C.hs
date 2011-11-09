module Language.Passage.Lang.C where

import Language.Passage.Utils hiding (double,int)
import Data.Word

--------------------------------------------------------------------------------
-- Generating C
--------------------------------------------------------------------------------

type CExpr    = Doc
type CStmt    = Doc
type CType    = Doc
type CDecl    = Doc
type CFunDecl = Doc
type CIdent   = Doc


ident      :: String -> CIdent
ident       = text

call       :: CExpr -> [CExpr] -> CExpr
call f xs   = f <> commaSep xs


cast       :: CType -> CExpr -> CExpr
cast t e    = parens t <+> parens e

arr_ix     :: CExpr -> CExpr -> CExpr
arr_ix a i  = a <> brackets i

var        :: CIdent -> CExpr
var x       = x

double_lit :: Double -> CExpr
double_lit  = text . show

int_lit    :: Int -> CExpr
int_lit     = pp

unsigned_lit :: Word -> CExpr
unsigned_lit = pp

string_lit  :: String -> CExpr
string_lit   = text . show

-- could be an array, in general
addr_of     :: CIdent -> CExpr
addr_of x    = char '&' <> x

main_name   :: CIdent
main_name   = ident "main"


double     :: CType
double      = text "double"

void       :: CType
void        = text "void"

int        :: CType
int         = text "int"

unsigned   :: CType
unsigned    = text "unsigned"

unsigned_long :: CType
unsigned_long  = text "unsigned long"

array_decl  :: CType -> CIdent -> [Int] -> CDecl
array_decl t a ds = t <+> a <> hcat (map (brackets . pp) ds)

extern     :: CDecl -> CDecl
extern d    = text "extern" <+> d

static     :: CDecl -> CDecl
static d    = text "static" <+> d

static_fun :: CFunDecl -> CDecl
static_fun d  = text "static" $$ d

fun_decl  :: CType -> CIdent -> [(CType, CIdent)] -> CFunDecl
fun_decl res f args = res <+> f <> args_decl
  where
  args_decl = case args of
                [] -> parens (text "void")
                _  -> commaSep [ t <+> x | (t, x) <- args ]

var_decl    :: CType -> CIdent -> CDecl
var_decl t x = t <+> x

-- In C this is an expr, but we don't need this "flexibility".
assign     :: CExpr -> CExpr -> CStmt
assign x y = x <+> text "=" <+> y <> semi

creturn    :: CExpr -> CStmt
creturn x   = text "return" <+> x <> semi

switch    :: CExpr -> [(Int,[CStmt])] -> CStmt -> CStmt
switch e as d =
  text "switch" <+> parens e <+> char '{'
    $$ nest 2 ( vcat (map ppCase as)
             $$ text "default:" <+> d)
    $$ char '}'
  where ppCase (i,s) = text "case" <+> int_lit i <> char ':' <+> vcat s

cbreak      :: CStmt
cbreak      = text "break" <> semi


callS      :: CExpr -> [CExpr] -> CStmt
callS f xs  = call f xs <> semi

block     :: [CStmt] -> CStmt
block ss   = char '{' $+$ nest 2 (vcat ss) $+$ char '}'

declBlock :: [CDecl] -> [CStmt] -> CStmt
declBlock ds ss = char '{' $+$ nest 2 (vcat ds $+$ vcat ss) $+$ char '}'

pragma    :: String -> CStmt
pragma x    = text "#pragma" <+> text x

