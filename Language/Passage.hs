{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Passage (

   -- * Types
   BayesianNetwork, BayesianGraph(..), StoVar(..), Node, BayesianSimulator

   -- * Constructing models
   , logGamma, using, tconst
   , vector, matrix, nodeArray, Vector, Matrix, NodeArray
   , tcase
   , (//)

   -- * Distributions
   , module UI

   -- * Extracting graphs
   , buildBayesianGraph

   -- * Displayihng graphs
   , PP(..), LaTeX(..)

   -- * Simulation
   , simulate, genSimulator
   , setSampleCount
   , setIterationsPerSample
   , setWarmupCount
   , setThreadNum
   , useMersenneTwister
   , enableProfiling
   , setRandomSeed
   , useSpecialSlicers
   , splitFiles
   , model, observe, monitor, monitorVec, monitorVecs

   -- * LaTeX
   , runLatex
  ) where

import Language.Passage.AST
import Language.Passage.UI as UI
import Language.Passage.Graph
import Language.Passage.Lang.LaTeX(LaTeX(..))
import qualified Language.Passage.Lang.LaTeX as LaTeX
import Language.Passage.Term
import Language.Passage.Utils
import Language.Passage.SimulatorConf

import qualified Language.Passage.Graph2C as C

import Control.Exception (finally)
import Control.Monad(when)

import System.Process(rawSystem, readProcess)
import System.Exit(ExitCode(..))
import System.Info(os)
import System.FilePath
import System.IO(openFile,hPutStrLn,hClose,IOMode(..))
import System.Directory(removeDirectoryRecursive, doesDirectoryExist)
import Paths_passage (getDataDir)

-- | Like monitor, but adds the indexes in the label of the variable.
monitorVec :: String -> Matrix -> [Int] -> BayesianSimulator ()
monitorVec name m xs = monitor lab (m (map fromIntegral xs))
  where
  lab = name ++ concatMap ix xs
  ix x = "[" ++ show x ++ "]"

monitorVecs :: String -> NodeArray -> [[Int]] -> BayesianSimulator ()
monitorVecs name m = mapM_ (monitorVec name m) 

type Node = Expr

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f =
  do dir <- init `fmap` readProcess "mktemp" ["-d","-t","bayesiandsl.XXXXXX"] ""
     -- init drops \n
     f dir `finally` removeDirectoryRecursive dir

runLatex :: BayesianNetwork a -> IO ()
runLatex t = withTempDir $ \dir ->
  do let file     = dir </> "out"
         tex_file = file <.> ".tex"
         pdf_file = file <.> ".pdf"
     writeFile tex_file (show doc)
     runCmd make_pdf ["-output-directory", dir, tex_file]
     runCmd show_pdf (pdf_args ++ [pdf_file])

  where
  doc = vcat [ LaTeX.cmd "documentclass" [ text "article" ]
             , LaTeX.env "document" [] (latex (snd (buildBayesianGraph t)))
             ]

  (show_pdf, pdf_args)
    | os == "linux" = ("evince",[])
    | otherwise     = ("open",["-W"])

  make_pdf          = "pdflatex"



runCmd :: String -> [String] -> IO ()
runCmd f as =
  do res <- rawSystem f as
     case res of
       ExitSuccess -> return ()
       ExitFailure n ->
        fail $ "(error " ++ show n ++ ") Failed to execute " ++ show f
                                          ++ " with arguments " ++ show as



createSimProject :: FilePath -> SimState -> C.SamplerConf -> IO ()
createSimProject dir st conf =
  do yes <- doesDirectoryExist dir
     when yes $ error $ "Directory: " ++ show dir ++ " already exists."

     -- Copy templates
     putStrLn $ "Creating directory " ++ show dir
     dataDir <- getDataDir
     let rt = dataDir </> "cbits" </> "runtime"
     runCmd "cp" [ "-r", rt, dir ]

     -- Create additional settings
     let src_dir       = dir </> "src"
         extra_settings = src_dir </> "extra_settings"
     hExtra <- openFile extra_settings WriteMode
     hPutStrLn hExtra "# Here one can put additional settings for the build"
     when (cfgMersenne st) $ hPutStrLn hExtra "CPPFLAGS+=D__USE_MERSENNE"
     when (cfgProfile st)  $ hPutStrLn hExtra "CFLAGS+=-pg -g"

     hClose hExtra

     -- Generate simulator
     putStrLn "Generating sampler."
     -- let c_file        = src_dir </> "sampler" <.> ".c"
     mapM_ (\(f,d) -> writeFile (src_dir </> f) (show d)) $ C.gen_c conf
     putStrLn $ "Generated C project: " ++ show src_dir

     -- generate R driver
     let rDriver = dir </> "histogram.R"
     writeFile rDriver (genR dir (zip [1..] (map fst (C.monitor conf))))
     putStrLn $ "Generated sample R cmds: " ++ show rDriver

-- TODO: Generate an R driver that knows what's being observed
genR :: String -> [(Int, String)] -> String
genR name labs = unlines $ [ "library(MASS)"
                      , "pdf(file='sample.pdf')"
                      , "table <- read.table('datafile')"
                      ] ++ map genHist labs
  where
  genHist (i, s) =
    "truehist(table[," ++ show i ++ "], xlab='" ++ s ++
          "', main='" ++ name ++ "')"





createSimulator :: FilePath -> SimState -> IO ()
createSimulator path st =
  case cfgNetwork st of
    Nothing -> error $ "No bayesian-network specified; please use \"bayesianNetwork\" to specify one."
    Just t  ->
      case cfgMonitor st of
        [] -> error $ "No montitors added; please use \"monitor\" to specify some."
        ms ->
          let conf = C.SamplerConf { C.graph        = t
                                   , C.sampleNum    = cfgSampleNum st
                                   , C.itsPerSample = cfgItsPerSample st
                                   , C.warmup       = cfgWarmup st
                                   , C.seed         = cfgRandomSeed st
                                   , C.observe      = cfgObserve st
                                   , C.initialize   = cfgInitialize st
                                   , C.monitor      = reverse ms
                                   , C.thread_num   = cfgThreadNum st
                                   , C.special_slicers = cfgSpecialSlicers st
                                   , C.split_files  = cfgSplitFiles st
                                   }
          in createSimProject path st conf

genSimulator :: FilePath -> BayesianSimulator () -> IO ()
genSimulator f b = createSimulator f (runSim b) >> return ()

simulate :: FilePath -> BayesianSimulator () -> IO ()
simulate f b = do createSimulator f (runSim b)
                  putStrLn "Running the simulation.."
                  runCmd "make" [ "--quiet", "-C", f ]
                  putStrLn "Done."



