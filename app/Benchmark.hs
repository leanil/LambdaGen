{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Benchmark where

import Compile
import Generate.Contraction
import Generate.ContractionText
import Test.ContractionTest
import Test.FunctionalTest
import Utility
import Control.Applicative ((<$>))
import Control.Comonad (extract)
import Control.Monad (forM_, replicateM, void)
import Control.Monad.State (StateT, evalStateT, lift, gets, modify)
import Data.Text (Text, pack, stripEnd)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStr, putStrLn, writeFile)
import NeatInterpolation
import System.FilePath ((</>),(<.>))

benchmarkCount :: Int
benchmarkCount = 10
contIds :: [Int]
contIds = [1..benchmarkCount]
sizes :: [Int]
sizes = iterateN 6 (*2) 8

main :: IO ()
main = do
    resetDir ("benchmark"</>"contraction")
    resetDir ("benchmark"</>"build")
    --exprs <- replicateM benchmarkCount sampleSimple
    --exprs <- loadContEqs ("experiment" </> "benchmark" <.> "json")
    let exprs = [t211, t212]
    putStrLn "Benchmarked contractions:"
    --saveContEqs ("benchmark"</>"contraction"</>"expressions"<.>"json") exprs
    let configs = [(i,j)|i<-[benchmarkToLambdaGen,compileLoopEval],j<-exprs]
    (incs, funs, regs) <- evalStateT (unzip3 . concat <$> mapM (uncurry makeBenchmarks) configs) 0
    T.writeFile ("benchmark"</>"main"<.>"cpp") $ testCode (T.concat incs) (T.concat funs) (T.concat regs)
    let runProc = createProcessAndExitOnFailure $ "benchmark" </> "build"
    runProc "cmake" ["-DCMAKE_BUILD_TYPE=Release", ".."]
    runProc "cmake" ["--build", ".", "--config", "Release"]

testCode :: Text -> Text -> Text -> Text
testCode includes benchFuns regs = purge [text|
    $includes
    #include "util.hpp"
    #include <benchmark/benchmark.h>
    #include <map>
    #include <string>

    $benchFuns

    $regs

    BENCHMARK_MAIN();|]

include :: Text -> Text -> Text
include ext evalId = [text|#include "$evalId.$ext"|]

contCase :: Int -> Int -> Text
contCase (tshow -> caseNum) (tshow -> n) = [text|case $caseNum: return !cont${n}test();|]

type Compiler = FilePath -> Text -> Extents -> ContEq -> IO ()

makeBenchmarks :: Compiler -> ContEq -> StateT Int IO [(Text,Text,Text)]
makeBenchmarks compiler expr = do
    num <- gets tshow
    modify (+1)
    let pretty = printContraction False expr
        makeBench size = do
            let s = tshow size
                inc = include "h" [text|cont${num}_$s|]
                benchFun = benchmarkFunction num s $ initData (const size) expr
                register = [text|BENCHMARK(cont${num}_${s}_benchmark)->ComputeStatistics("min", min_time);|]
            compiler ("benchmark" </> "contraction") (stripEnd [text|cont${num}_$s|]) (const size) expr
            return (inc,benchFun,register)
    lift $ T.putStr [text|$num) $pretty|]
    lift $ mapM makeBench sizes
       
benchmarkToLambdaGen :: Compiler
benchmarkToLambdaGen path kernelName sizes expr = 
    void $ compile path kernelName $ translate sizes expr

benchmarkFunction :: Text -> Text -> Text -> Text
benchmarkFunction num size init = [text|
    void cont${num}_${size}_benchmark(benchmark::State& state) {
        $init
        for (auto _ : state) {
            cont${num}_$size(userData);
        }
        free_data(userData);
    }|]