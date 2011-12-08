{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Passage.Graph2C where

import Language.Passage.Utils hiding (double,int)
import Language.Passage.Term hiding (bin)
import Language.Passage.AST
import Language.Passage.Lang.C
import Language.Passage.GraphColor(groupByColor)


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe(maybeToList,fromJust)
import Data.List(sortBy, transpose, foldl')
import Data.Function(on)
import MonadLib (ReaderT, StateT, Id
                , runId, runStateT, runReaderT
                , get, set, asks, mapReader
                , forM
                , zipWithM
                )
import Data.List (partition)

import Data.Graph(SCC(..))
import Data.Graph.SCC
import Debug.Trace


--------------------------------------------------------------------------------
-- Compilation of expressions
--------------------------------------------------------------------------------

cnameVar :: (NodeIdx, StoVar) -> M CExpr
cnameVar (ix,sv) =
  case stoVarName sv of
    InArray x is ->
      do ai <- lookupArray x
         let fixIndex i (from,_) = int_lit (i - from)
             name = arrName (x,ai)
         return $ foldl arr_ix (var name)
                $ zipWith fixIndex is $ arrayDimensions ai
    _ -> return $ var $ simpleCName ix


simpleCName :: NodeIdx -> CIdent
simpleCName x = ident ("v_" ++ show x)

variable :: NodeIdx -> M CExpr
variable x =

  -- Is this an observed variable?
  do isObs <- isObserved x
     case isObs of
       Just v -> return (double_lit v)      -- Yes, just use the known value.
       Nothing ->

         {- Are we compiling within the LL_FUN for this variable?
            When we generate the LL_FUN for a stoachastic variable,
            we always use it's simple name, even if the variable is stored
            in an array in the long run.   The reason for this is that in
            the LL_FUN, the variable is passed as an argument. -}
         do samp <- isSampled x
            if samp
              then return $ var $ simpleCName x
              else

                -- OK, perhaps we have an ordinary stochastic variable?
                do mbsv <- lookupVarMb x
                   case mbsv of
                     Just sv -> cnameVar (x,sv)

                     -- Hmm, we don't know about this variable.
                     -- The variable must refer to a deterministic node
                     -- generated to factor repeated compution out
                     -- of an LL_FUN.
                     Nothing -> return $ var $ simpleCName x

term :: Term NodeIdx -> M CExpr
term t =
  case t of
    TVar x -> variable x
    TArr x -> do ai <- lookupArray x
                 return $ var $ arrName (x,ai)
    TConst x -> return (double_lit x)
    TApp op ts ->
      do ds@(a : bs) <- mapM term ts
         let b : _ = bs
             bin x = parens a <+> text x <+> parens b
         case op of
           TExp      -> return $ call (ident "exp") ds
           TLog      -> return $ call (ident "log") ds
           TNeg      -> return $ char '-' <> parens (head ds)
           TAdd      -> return $ bin "+"
           TMul      -> return $ bin "*"
           TSub      -> return $ bin "-"
           TDiv      -> return $ bin "/"
           TPow      -> case ts of
             [_ , TConst 2.0] -> (return $ call (ident "square") [a])
             _                -> return $ call (ident "pow") ds
           TLogGamma -> return $ call (ident "lgamma") ds

           TCase ->
             do i <- newDetVar -- Just used as a new name
                let name = ident ("case_fun_" ++ show i)

                -- if we are in the LL function for some variable,
                -- we have to pass the sampled variable to the "case" function.
                args <- (map simpleCName . maybeToList) `fmap` isSampling

                newLocalFunDecl
                  (fun_decl double name [ (double,x) | x <- args ]) [
                    switch (cast int a)
                      (zip [ 0 .. ] (map (return . creturn) bs))
                      (callS (var (ident "crash_out_of_bounds"))
                                                [ text "__LINE__" ])
                  ]
                return $ call (var name) (map var args)

           TIx       ->
             case ts of
               [ arr, ix ] ->
                  do dims <- getArrDimensions arr
                     case dims of
                       (from,_) : _ ->
                          do expr <- term (ix - fromIntegral from)
                             return (arr_ix a (cast int expr))
                       _ -> error $ "Type error: attempt ro index non an array."
               _ -> error $ "TIx: Unexpected args: " ++ show ts

getArrDimensions :: Term NodeIdx -> M [(Int,Int)]
getArrDimensions t =
  case t of
    TArr x -> arrayDimensions `fmap` lookupArray x
    TApp TIx [ a, _ ] ->
      do ds <- getArrDimensions a
         case ds of
           _ : ds1 -> return ds1
           [] -> error $ "Type error: attempt to index a non-array."
    _ -> error $ "Type error: not an array"

--------------------------------------------------------------------------------

newtype M a = M (ReaderT R (StateT S Id) a) deriving (Functor, Monad)

data R  = R { config   :: SamplerConf
            , sampling :: Maybe NodeIdx
            }

data CModule =
  CModule
    { cpp_stuff :: Doc          -- ^ Includes, #define, etc.
    , var_decls :: [Doc]        -- ^ Variable declarations
    , cpp_funs  :: Doc          -- ^ #included templtes
    , fun_decls :: [(Doc,Doc)]  -- ^ Function declarations: decl, body
    }

blankMod :: CModule
blankMod = CModule { cpp_stuff = empty
                   , var_decls = []
                   , cpp_funs = empty
                   , fun_decls = []
                   }

-- XXX: Watch out with the ++ing here...
mergeCModules :: CModule -> CModule -> CModule
mergeCModules m1 m2 =
  CModule { cpp_stuff = cpp_stuff m1 $$ cpp_stuff m2
          , var_decls = var_decls m1 ++ var_decls m2
          , cpp_funs  = cpp_funs m1 $$ cpp_funs m2
          , fun_decls = fun_decls m1 ++ fun_decls m2
          }



data S  = S { main_mod    :: CModule
            , cur_mod     :: Maybe CModule
            , helper_mods :: [(NodeIdx, CModule)]
            , cnames      :: !Int         -- ^ Name supply
            }

noHelpers :: S -> S
noHelpers s = s { main_mod = foldr mergeCModules (main_mod s)
                           $ map snd (helper_mods s)
                , helper_mods = []
                }

getGraph :: M BayesianGraph
getGraph = M (asks (graph . config))

lookupArray :: NodeIdx -> M ArrayInfo
lookupArray x =
  do mb <- lookupArrayMb x
     case mb of
       Just a  -> return a
       Nothing -> error ("Unknown array variable: " ++ show x)


lookupArrayMb :: NodeIdx -> M (Maybe ArrayInfo)
lookupArrayMb x =
  do g <- getGraph
     return (IM.lookup x (stoArryas g))

lookupVarMb :: NodeIdx -> M (Maybe StoVar)
lookupVarMb x =
  do g <- getGraph
     return (IM.lookup x (stoNodes g))

lookupVar :: NodeIdx -> M StoVar
lookupVar x =
  do mb <- lookupVarMb x
     case mb of
       Just sv -> return sv
       Nothing -> error ("Unknown stochastic variable: " ++ show x)

nowSampling :: NodeIdx -> M a -> M a
nowSampling x (M a) = M (mapReader (\i -> i { sampling = Just x }) a)

isSampling :: M (Maybe NodeIdx)
isSampling = M (asks sampling)

isSampled :: NodeIdx -> M Bool
isSampled x = (Just x ==) `fmap` isSampling

isObserved :: NodeIdx -> M (Maybe Double)
isObserved ix = IM.lookup ix `fmap` M (asks (observe . config))

isInitialized :: NodeIdx -> M (Maybe Double)
isInitialized ix = IM.lookup ix `fmap` M (asks (initialize . config))

newDetVar :: M NodeIdx
newDetVar = M $
  do s <- get
     let i = cnames s
     set s { cnames = i + 1 }
     return i

newHelper :: NodeIdx -> M a -> M a
newHelper i (M m) = M $
  do s <- get
     set s { cur_mod = Just blankMod }
     a <- m
     s1 <- get
     set s1 { cur_mod = Nothing
            , helper_mods = (i, fromJust (cur_mod s1)) : helper_mods s1
            }
     return a

updHelper :: (CModule -> CModule) -> M ()
updHelper f = M $
  do s <- get
     case cur_mod s of
       Nothing -> error "BUG: updHelper called without a module"
       Just m ->
         set s { cur_mod = Just (f m) }

updMain :: (CModule -> CModule) -> M ()
updMain f = M $
  do s <- get
     set s { main_mod = f (main_mod s) }


-- add a new function to the main module.
newFunDecl :: CFunDecl -> [CStmt] -> M ()
newFunDecl d body =
  updMain $ \m -> m { fun_decls = (d, block body) : fun_decls m }

-- add a new declaration to the main module.
newDecl :: CDecl -> M ()
newDecl d = updMain $ \m -> m { var_decls = d : var_decls m }


-- Add "cpp" includes to the main module
cpp :: String -> M ()
cpp t = updMain $ \m -> m { cpp_stuff = cpp_stuff m $$ text t }




-- add a new function to the current helper module
newLocalFunDecl :: CFunDecl -> [CStmt] -> M ()
newLocalFunDecl d body = updHelper $ \m ->
  m { fun_decls = (d, block body) : fun_decls m }

-- add a new static variable to the current helper module
newLocalDecl :: CDecl -> M ()
newLocalDecl d = updHelper $ \m ->
  m { var_decls = static d : var_decls m }


cppFun :: String -> M ()
cppFun t = cppFun' (text t)

-- Add "#include function" to the current helper module
cppFun' :: Doc -> M ()
cppFun' t = updHelper $ \m ->
  m { cpp_funs = cpp_funs m $$ t }



runM :: SamplerConf -> M a -> (a, S)
runM conf (M m) = runId $ runStateT start $ runReaderT info m
  where start = S { main_mod    = blankMod
                  , cur_mod     = Nothing
                  , helper_mods = []
                  , cnames      = maxNode + 1
                  }
        info  = R { config = conf, sampling = Nothing }
        (maxNode,_) = IM.findMax $ stoNodes $ graph conf


renderMod :: CModule -> Doc
renderMod m =
  cpp_stuff m $$
  char ' '    $$ text "/* Variable declarations */" $$
  decls (var_decls m) $$
  char ' '    $$ text "/* Function types */" $$
  decls (map fst (fun_decls m)) $$
  char ' '    $$ text "/* Included templates */" $$
  cpp_funs m $$
  char ' '    $$ text "/* Function definitions */" $$
  vcat [ d $$ b | (d,b) <- fun_decls m ]

  where decls = vcat . map (\d -> d <> semi)


renderState :: SamplerConf -> S -> [(FilePath, Doc)]
renderState conf s0 = ("sampler.h", hdr)
                    : ("sampler.c", main)
                    : map helper (helper_mods s)
  where
  s       = if split_files conf then s0 else noHelpers s0
  mm      = main_mod s
  hdr     = decls (map extern (var_decls mm))
  main    = renderMod mm {
              var_decls = concatMap extern_helper (helper_mods s) ++

                                                              var_decls mm }
  helper (i,m) =
    ( "slice_" ++ show i ++ ".c"
    , renderMod $ m { cpp_stuff = text "#include \"passage.h\"" $$
                                  text "#include \"sampler.h\"" }
    )

  extern_helper (h,m)
    | special_slicers conf
       = [ text ("extern double SLICE(" ++ show h ++ ")(double)")
         , text ("extern double SLICE_TUNE(" ++ show h ++ ")(double)")
         ]
    | otherwise = map (extern . fst) (fun_decls m)

  decls   = vcat . map (\d -> d <> semi)



--------------------------------------------------------------------------------

call_slicer :: (NodeIdx, StoVar) -> M ([CStmt], CExpr,CExpr)
call_slicer x =
  do special <- M (asks (special_slicers . config))
     if special then call_special_slicer x else call_generic_slicer x



call_generic_slicer :: (NodeIdx, StoVar) -> M ([CStmt], CExpr,CExpr)
call_generic_slicer (ix,sv) =
  do v <- cnameVar (ix,sv)
     case priSupport (stoVarPrior sv) of
       Real ->
         do newDecl $ var_decl double wid
            let slice = var (ident "slice_real")
                tune  = var (ident "tune_slice_real")
            return
              ( [ assign (var wid) (double_lit 1) ]
              , call slice [ llfun, var wid,     v ]
              , call tune  [ llfun, addr_of wid, v ]
              )

       PosReal ->
         do newDecl $ var_decl double wid
            let z = int_lit 0
                slice = var (ident "slice_pos_real")
                tune  = var (ident "tune_slice_pos_real")
            return
              ( [ assign (var wid) (double_lit 1) ]
              , call slice [ llfun, var wid,     z, v ]
              , call tune  [ llfun, addr_of wid, z, v ]
              )

       Interval lo hi ->
         do e1 <- term lo
            e2 <- term hi
            let slice = var (ident "slice_real_left_right")
                expr = call slice [ llfun, e1, e2, v ]
            return ([], expr, expr)

       Discrete (Just t) ->
         do e <- term t
            let slice = var (ident "slice_discrete_right")
                expr = call slice [ llfun, e, v ]
            return ([], expr, expr)

       Discrete Nothing ->
         do let slice = var (ident "slice_discrete")
                expr  = call slice [ llfun, v ]
            return ([], expr, expr)

  where llfun = var $ ident $ "LL_FUN(" ++ show ix ++ ")"
        wid   = ident $ "WIDTH(" ++ show ix ++ ")"


call_special_slicer ::  (NodeIdx, StoVar) -> M ([CStmt], CExpr,CExpr)
call_special_slicer (ix,sv) =
  do v <- cnameVar (ix,sv)
     cppFun ("#define VAR " ++ show ix)

     let fun      = var $ ident $ "SLICE(" ++ show ix ++ ")"
         the_tune_fun = var $ ident $ "SLICE_TUNE(" ++ show ix ++ ")"

     tune_fun <- case priSupport (stoVarPrior sv) of
       Real ->
         do cppFun "#include \"templates/slice.c\""
            return the_tune_fun

       PosReal ->
         do cppFun "#define LEFT 0"
            cppFun "#include \"templates/slice.c\""
            cppFun "#undef LEFT"
            return the_tune_fun

       Interval lo hi ->
         do e1 <- term lo
            e2 <- term hi
            cppFun' (text "#define LEFT " <+> parens e1)
            cppFun' (text "#define RIGHT " <+> parens e2)
            cppFun "#include \"templates/slice.c\""
            cppFun "#undef LEFT"
            cppFun "#undef RIGHT"
            return fun

       Discrete (Just t) ->
         do e <- term t
            cppFun' (text "#define RIGHT" <+> parens e)
            cppFun "#include \"templates/finiteMetropolis.c\""
            cppFun "#undef RIGHT"
            return fun

       Discrete Nothing ->
         do cppFun "#include \"templates/metropolis_posreal.c\""
            return fun

     cppFun "#undef VAR"
     return ( []
            , call fun [v]
            , call tune_fun [v]
            )



initOrder :: [(NodeIdx, StoVar)] -> M [(NodeIdx, StoVar)]
initOrder ns =
  do bg <- getGraph
     return $ map check $ stronglyConnComp [ (n,ix,uses bg v) | n@(ix,v) <- ns ]
  where
  uses bg = IS.toList . fvsSupport (fvsArray bg) . priSupport . stoVarPrior

  check (AcyclicSCC d)  = d
  check (CyclicSCC _)   = error "Cannot initialize: recursive support!"

init_code :: (NodeIdx, StoVar) -> M CStmt
init_code (x,sv) =
  do v <- cnameVar (x,sv)
     i <- case priSupport (stoVarPrior sv) of
           Real       -> return $ double_lit 0
           Discrete _ -> return $ double_lit 0
           PosReal    -> return $ double_lit 1
           Interval lo hi -> term (lo + (hi - lo) / 2)
            -- duplicates lo but, hopefully, this does not matter too much

     return (assign v i)

{- If an observed variable is stored in an array, then we need to initialize
   the corresponding entry in the array with the observed value.  The reason
   for this is that there may be expressions of the form: a[i], with "a"
   begin an observed array, and "i" which is not statically known.
-}
init_code_initialized :: (NodeIdx, Double) -> M [CStmt]
init_code_initialized (x,d) =
  do sv <- lookupVar x
     case stoVarName sv of
       InArray {} ->
         do v <- cnameVar (x,sv)
            return [assign v (double_lit d)]
       _ -> return []

--------------------------------------------------------------------------------

ll_summand :: (Term NodeIdx, Term NodeIdx)
           -> M ([CStmt], Term NodeIdx, IS.IntSet)
ll_summand (x,c) =
  do bg <- getGraph
     if isSimpleTerm c
       then return ([], x * c, varsOf bg (x * c))
       else do c1 <- newDetVar
               let c2 = simpleCName c1
               newLocalDecl $ var_decl double c2
               expr <- term c
               return ( [assign (var c2) expr]
                      , x * tvar c1
                      , varsOf bg c `IS.union` varsOf bg x
                      )
  where
  varsOf bg = leavesOfTerm (fvsArray bg)

data StoVarCode =
  StoVarCode
    { tuneCode  :: [CStmt]
    , sliceCode :: [CStmt]
    , locality  :: (Int,[Int])  -- array number, indexes.
                                -- clobals would be a 1-dim array
                                -- call "-1".  (XXX)
    }

sto_var :: (NodeIdx, StoVar) -> M ( [CStmt] -- init code
                                  , (NodeIdx, StoVarCode, IS.IntSet)
                                  )
sto_var (ix,sv) = newHelper ix $
  do let xParam = simpleCName ix
         iname  = ident ("INIT_DET_VARS(" ++ show ix ++ ")")
         llname = ident $ "LL_FUN(" ++ show ix ++ ")"

     -- Here we compute a "locality" for the variable.
     -- This is useful when we group work by thread because
     -- we prefer to put close updates together.
     loc <- case stoVarName sv of
              InArray aix ixes -> return (aix,ixes)
              _ -> do newDecl $ var_decl double xParam
                      return (-1,[0])   -- XXX: count which vars are close

     (is,ts,vs) <- unzip3 `fmap` mapM ll_summand (M.toList (stoPostDistLL sv))
     expr <- nowSampling ix (term (sum ts))
     init_dets <- case concat is of
                    [] -> return []
                    have_dets ->
                      do newLocalFunDecl (fun_decl void iname []) have_dets
                         return [ callS (var iname) [] ]
     newLocalFunDecl (fun_decl double llname [(double,xParam)]) [ creturn expr ]
     x    <- cnameVar (ix,sv)
     (initW, sliceExpr,sliceTuneExpr) <- call_slicer (ix,sv)
     ic   <- init_code (ix,sv)
     return ( initW ++ [ic]  -- initializaztion code
            , ( ix
              , StoVarCode
                  { tuneCode = init_dets ++  [ assign x sliceTuneExpr ]
                  , sliceCode = init_dets ++ [ assign x sliceExpr ]
                  , locality  = loc
                  }
              , IS.unions vs                            -- sto. var. deps.
              )
            )


--------------------------------------------------------------------------------

data SamplerConf = SamplerConf
  { graph         :: BayesianGraph
  , sampleNum     :: Int
  , itsPerSample  :: Int
  , warmup        :: Int
  , thread_num    :: Int
  , seed          :: [Int]
  , monitor       :: [(String, Term NodeIdx)]
  , observe       :: IM.IntMap Double  -- Map node indexes to observed values.
  , initialize    :: IM.IntMap Double  -- Map node indexes to initialized values.
  , special_slicers :: Bool     -- Generate a custom slicer per variable?
  , split_files     :: Bool     -- Make one file per variable?
  }

declareArr :: (NodeIdx, ArrayInfo) -> M ()
declareArr (ix,i) =
  newDecl $ array_decl double (arrName (ix,i)) (map size (arrayDimensions i))
    where size (x,y) = y - x + 1

arrName :: (NodeIdx,ArrayInfo) -> CIdent
arrName (x,_) = ident ("a_" ++ show x)

{-
genParGroups :: Int -> (StoVarCode -> [CStmt]) -> [[StoVarCode]] -> [CStmt]
genParGroups cpus which xs = concatMap makeSections xs
  where
  entries_per_thread len  = (len + cpus - 1) `div` cpus

  makeSections vs = [ pragma "omp sections"
                    , block $ concatMap makeSection
                            $ chunks (entries_per_thread len)
                            $ sortBy (compare `on` locality) vs
                    ]
    where len = length vs

  makeSection vs  = [ pragma "omp section"
                    , block (concatMap which vs)
                    ]

-}

isSingleton [_] = True
isSingleton _   = False

singleton x = [x]

intTable :: [Int] -> [(Int,Int)]
intTable xs = IM.assocs $! foldl' f IM.empty xs
    where f m x = let  m' = IM.insertWith (+) x 1 m
                       Just v = IM.lookup x m'
                  in v `seq` m'


genParGroups :: Int -> (StoVarCode -> [CStmt]) -> [[StoVarCode]] -> [[CStmt]]
genParGroups cpus which colors = trace (show lengths) . map concat . transpose $ seqBlocks ++ parBlocks

  where
  lengths = intTable . map length $ colors
  (seqColors, parColors) = partition isSingleton colors
  
  seqBlocks = singleton . addBlanks cpus . singleton . makeBlock . concat $ seqColors
  
  parBlocks = map threadBlocks parColors
  
  entries_per_thread len  = (len + cpus - 1) `div` cpus

  -- Allocate a list of indipendent statements to different threads.
  -- Each blocks start with a barrier
  threadBlocks :: [StoVarCode] -> [[CStmt]]
  threadBlocks vs = map makeBlock
                  $ addBlanks cpus
                  $ chunks (entries_per_thread len)
                  $ sortBy (compare `on` locality) vs
    where len = length vs

  makeBlock :: [StoVarCode] -> [CStmt]
  makeBlock vs  = pragma "omp barrier" : concatMap which vs

  -- If there is not enough work for all threads, we insert
  -- an empty list, so that we still get a barrier, otherwise
  -- things get our of sync.
  addBlanks n []        = replicate n []
  addBlanks n (x : xs)  = x : seq m (addBlanks m xs)
    where m = n - 1



-- Split a list into chunks of the given length.
-- If we run out of elements we make empty lists.
chunks :: Int -> [a] -> [[a]]
chunks n xs = case splitAt n xs of
                (as,bs) -> as : case bs of
                                  [] -> []
                                  _  -> chunks n bs



genThread :: [(CStmt,CStmt)] -> Int -> ([CStmt], [CStmt]) -> M [CStmt]
genThread monitor_code n (tune_code,sample_code) =
  do newFunDecl (fun_decl void name []) $
       [ var_decl unsigned_long (ident "i") <> semi
       , var_decl unsigned_long (ident "j") <> semi
       ]

       ++
       ifMaster ( map fst monitor_code ++
                  [ nl, toStdErr [ string_lit "Tuning width parameters.\n" ] ]
                )

       ++
       [ text ("for (i = 0; i < warm_up_steps; ++i)")
           $$ nest 2 (block tune_code)
       , barrier
       ]

       ++
       ifMaster [ toStdErr [string_lit "Sampling.\n"] ]

       ++
       [ text ("for (i = 0; i < number_of_samples; ++i)") $$
           nest 2 (block $
               ifMaster [ ppProg ]

               ++
               [ text ("for (j = 0; j < steps_per_sample; ++j)")
                   $$ nest 2 (block sample_code)
               , barrier
               ]

               ++
               ifMaster (printRowLabel : map snd monitor_code ++ [ nl ])
               )
       ]
     return [ pragma "omp section"
            , callS name []
            ]

  where
  name        = ident ("thread_" ++ show n)
  ifMaster xs = if n == 0 then xs else []
  barrier     = pragma "omp barrier"
  toStdErr xs = callS (var (ident "fprintf")) (var (ident "stderr") : xs)
  toStdOut xs = callS (var (ident "printf")) xs
  ppProg      = callS (ident "progress") [ var (ident "i") ]

  printRowLabel         = toStdOut [ string_lit "%lu", ident "i" ]
  nl                    = toStdOut [ string_lit "\n" ]







gen_c :: SamplerConf -> [(FilePath, Doc)]
gen_c conf = renderState conf $ snd $ runM conf $
  do let bg = graph conf
     cpp "#include <math.h>"
     cpp "#include <stdio.h>"
     cpp "#include <omp.h>"
     cpp "#include \"passage.h\""

     mapM_ declareArr $ IM.toList $ stoArryas bg

     -- We generate sampling code only for stochastic variables that
     -- are not observed:
     let observedVars = IM.keysSet (observe conf)
         sampledNodes = filter (not . (`IS.member` observedVars) . fst)
                      $ IM.toList $ stoNodes bg
     (ins,deps) <- unzip `fmap` (mapM sto_var =<< initOrder sampledNodes)
     let dropObserved (x,y,zs) =
            (x,y, IS.filter (not . (`IS.member` observedVars)) zs)
         par_groups = map (map snd) $ groupByColor $ map dropObserved deps

     -- Observed stochastic variables just get initialized once.
     -- Variables that are not in an array don't even need to be initialized
     -- but it is important the we initialize arrays, because of expressions
     -- a[i], where "a" is observed but "i" is not.
     obs_ins  <- mapM init_code_initialized $ IM.toList $ observe conf

     init_ins <- mapM init_code_initialized $ IM.toList $ initialize conf

     let cpus = thread_num conf

     newFunDecl (fun_decl void (ident "set_defaults") []) $
        [ assign (var (ident "number_of_samples")) $ int_lit $ sampleNum conf
        , assign (var (ident "steps_per_sample"))  $ int_lit $ itsPerSample conf
        , assign (var (ident "warm_up_steps"))     $ int_lit $ warmup conf
        , assign (var (ident "num_threads"))       $ int_lit cpus
        , assign (var (ident "have_seed"))        $ int_lit $ length $ seed conf
        ] ++ [ assign (arr_ix (var (ident "seeds")) (int_lit n)) (int_lit v)
                  | (n,v) <- zip [ 0 .. ] (reverse (seed conf)) ]

     newFunDecl (fun_decl void (ident "init_vars") [])
        $ concat $ obs_ins ++ ins

     -- Code to print the values of monitored expressions.
     monitor_code <- forM (monitor conf) $ \(lab,x) ->
        do expr <- term x
           return ( callS (ident "printf") [ string_lit ("\t" ++ lab) ]
                  , callS (ident "printf") [ string_lit ("\t%f"), expr ]
                  )

     let tune_codes  = genParGroups cpus tuneCode  par_groups
         slice_codes = genParGroups cpus sliceCode par_groups

     threads <- zipWithM (genThread monitor_code)
                               [ 0 .. ] (zip tune_codes slice_codes)


     -- The main sampling function.
     newFunDecl (fun_decl void (ident "sampler") []) $
        [ pragma "omp sections"
        , block (concat threads)
        ]

