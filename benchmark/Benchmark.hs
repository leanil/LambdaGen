{-# LANGUAGE OverloadedStrings, QuasiQuotes, TupleSections, ViewPatterns #-}

import Compile
import Generate.Contraction
import Generate.ContractionText
import Test.ContractionTest
import Test.FunctionalTest
import Utility
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Comonad (extract)
import Control.Monad (forM_, replicateM, void, zipWithM)
import Control.Monad.State (StateT, evalStateT, lift, gets, modify)
import Data.Text (Text, pack, stripEnd)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStr, putStrLn, writeFile)
import NeatInterpolation
import System.FilePath ((</>),(<.>))

main :: IO ()
main = do
    resetDir ("benchmark"</>"build")
    resetDir ("benchmark"</>"contraction")
    exprs <- replicateM 1 $ sample (GenConfig 1 4 1 4 1 6 2 2) -- See GenConfig def. in Contraction.hs
    --exprs <- loadContEqs ("experiment" </> "benchmark" <.> "json")
    --let exprs = contTests
    let (possibleSizes, maxTotalElemCount) = ([2,4,5,7,8,32,48,50,64,100,250,512,1000,1024,1047], 200000)
    extents <- mapM (replicateM 5 . genExtents possibleSizes maxTotalElemCount) exprs
    saveBenchmarks ("benchmark"</>"data"</>"expressions"<.>"json") $ concat $ zipWith (\expr exts -> map (expr,) exts) exprs extents
    -- let sizes = iterateN 5 (*2) 8 -- every expression will be benchmarked with each of these sizes
    -- saveBenchmarks ("benchmark"</>"contraction"</>"expressions"<.>"json") [(i,const j)|i<-exprs,j<-sizes]
    putStrLn "Benchmarked contractions:"
    let makeBench expr exts = sizedBenchmarks (zip exts $ map tshow [0..]) benchmarkToLambdaGen expr
    (incs, funs, regs) <- evalStateT (unzip3 . concat <$> zipWithM makeBench exprs extents) 0
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

constSizedBenchmarks :: [Int] -> Compiler -> ContEq -> StateT Int IO [(Text,Text,Text)]
constSizedBenchmarks sizes = sizedBenchmarks $ map (const &&& tshow) sizes

sizedBenchmarks :: [(Extents,Text)] -> Compiler -> ContEq -> StateT Int IO [(Text,Text,Text)]
sizedBenchmarks sizes compiler expr = do
    num <- gets tshow
    modify (+1)
    let pretty = printContraction False expr
    lift $ T.putStr [text|$num) $pretty|]
    lift $ mapM (\(size,name) -> makeBenchmark compiler (stripEnd [text|cont${num}_$name|]) size expr) sizes

makeBenchmark :: Compiler -> Text -> Extents -> ContEq -> IO (Text,Text,Text)
makeBenchmark compiler name sizes expr = do
    let inc = include "h" name
        benchFun = benchmarkFunction name $ initData sizes expr
        register = [text|BENCHMARK(${name}_benchmark)->ComputeStatistics("min", min_time);|]
    compiler ("benchmark" </> "contraction") name sizes expr
    return (inc,benchFun,register)
       
benchmarkToLambdaGen :: Compiler
benchmarkToLambdaGen path kernelName sizes expr = 
    void $ compile path kernelName $ translate sizes expr

benchmarkFunction :: Text -> Text -> Text
benchmarkFunction name init = [text|
    void ${name}_benchmark(benchmark::State& state) {
        $init
        for (auto _ : state) {
            $name(userData);
        }
        free_data(userData);
    }|]