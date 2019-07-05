{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

import Compile
import Generate.Contraction
import Generate.ContractionText
import Test.ContractionTest
import Test.FunctionalTest
import Utility
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (replicateM, void, zipWithM_)
import Data.Text (Text, append, stripEnd)
import qualified Data.Text as T (concat, unwords)
import qualified Data.Text.IO as T (putStr, putStrLn, writeFile)
import NeatInterpolation
import System.FilePath ((</>),(<.>))

main :: IO ()
main = do
    let numExprs = 5
        numExtents = 5 -- number of different extent sets for each expression
        config = GenConfig { minDims = 1, maxDims = 4, minOperands = 1, maxOperands = 4,
                             minTensors = 1, maxTensors = 6, minSums = 1, maxSums = 1,
                             allowUnorderedIndexing = True, allowIrregularSums = False }
        (possibleSizes, maxTotalElemCount) = ([2,4,5,7,8,32,48,50,64,100,250,512,1000,1024,1047], (0,1000000))
        nums = map tshow [0..]
        chunk = chunkList 50
    resetDir ("benchmark"</>"build")
    resetDir ("benchmark"</>"src")
    --exprs <- replicateM numExprs $ sample config
    --exprs <- loadContEqs ("experiment" </> "benchmark" <.> "json")
    --let exprs = contTests
    --extents <- mapM (replicateM numExtents . genExtents possibleSizes maxTotalElemCount) exprs
    benchmarks <- loadBenchmarks "C:/Users/Andris/Documents/anomaly.json"
    let --benchmarks = concat $ zipWith3 (\expr exts name -> zipWith (\ext extName -> (expr,ext,stripEnd [text|cont${name}_$extName|])) exts nums) exprs extents nums
        benchNames = map thdOf3 benchmarks
    saveBenchmarks ("benchmark"</>"data"</>"expressions"<.>"json") benchmarks
    -- let sizes = iterateN 5 (*2) 8 -- every expression will be benchmarked with each of these sizes
    putStrLn "Benchmarked contractions:"
    mapM_ (\(expr,_,name) -> let pretty = printContraction False expr in T.putStr [text|$name) $pretty|]) benchmarks
    codeParts <- map unzip . chunk <$> mapM (uncurry3 $ makeBenchmark compileLoopEval) benchmarks
    let add_benchmark (T.unwords -> names) num = [text|add_benchmark($num $names)|]
    T.writeFile ("benchmark"</>"add_executables"<.>"txt") $ T.concat $ zipWith add_benchmark (chunk benchNames) nums
    zipWithM_ (\(funs,regs) num -> T.writeFile ("benchmark"</>"src"</>"main" ++ show num<.>"cpp") $ benchMainCode (map (include "h") benchNames) funs regs) codeParts [0..]
    let runProc = createProcessAndExitOnFailure $ "benchmark" </> "build"
    runProc "cmake" ["-DCMAKE_BUILD_TYPE=Release", ".."]
    runProc "cmake" ["--build", ".", "--config", "Release", "--parallel"]
    runProc "cmake" ["--build", ".", "--config", "Release", "--target", "data"]

data BenchConfig = BenchConfig {
    numExprs, numExtents :: Int,
    genConfig :: GenConfig,
    possibleSizes :: [Int],
    minTotalCost, maxTotalCost :: Int,
    compiler :: String,
    executableSize :: Int,
    buildThreads, runThreads :: Int
}

prepareBenchmarks :: BenchConfig -> [(ContEq,Extents)]
prepareBenchmarks = undefined

benchMainCode :: [Text] -> [Text] -> [Text] -> Text
benchMainCode (T.concat -> includes) (T.concat -> benchFuns) (T.concat -> regs) =
    purge [text|
        $includes
        #include "util.hpp"
        #include <benchmark/benchmark.h>
        #include <map>
        #include <string>

        $benchFuns

        $regs

        BENCHMARK_MAIN();
    |]

include :: Text -> Text -> Text
include ext evalId = [text|#include "$evalId.$ext"|]

contCase :: Int -> Int -> Text
contCase (tshow -> caseNum) (tshow -> n) = [text|case $caseNum: return !cont${n}test();|]

type Compiler = FilePath -> Text -> Extents -> ContEq -> IO ()

constSizedBenchmarks :: [Int] -> Compiler -> Text -> ContEq -> IO [(Text,Text)]
constSizedBenchmarks sizes compiler name expr =
    mapM (\size -> makeBenchmark compiler expr (const size) (name `append` "_" `append` tshow size)) sizes

makeBenchmark :: Compiler ->  ContEq -> Extents -> Text -> IO (Text,Text)
makeBenchmark compiler expr sizes name = do
    let benchFun = benchmarkFunction name $ initData sizes expr
        register = [text|BENCHMARK(${name}_benchmark)->ComputeStatistics("min", min_time);|]
    compiler ("benchmark" </> "src") name sizes expr
    return (benchFun,register)
       
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