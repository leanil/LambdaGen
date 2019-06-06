{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Benchmark where

import Compile
import Generate.Contraction
import Generate.ContractionText
import Test.FunctionalTest
import Utility
import Control.Applicative ((<$>))
import Control.Comonad (extract)
import Control.Monad ((>>=), foldM, forM)
import Data.Functor.Foldable (cata)
import Data.Text (Text, pack, stripEnd, unpack)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStr, putStrLn, writeFile)
import NeatInterpolation
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (FilePath, (</>),(<.>))
import System.IO (print)
import System.Process (cwd, createProcess, proc, waitForProcess)

benchmarkCount :: Int
benchmarkCount = 100
contIds :: [Int]
contIds = [1..benchmarkCount]
sizes :: [Int]
sizes = iterateN 5 (*2) 8

createProcessAndExitOnFailure :: String -> [String] -> IO ()
createProcessAndExitOnFailure processName args = do
    (_, _, _, handle) <- createProcess (proc  processName args){ cwd = Just "benchmark/build" }
    code <- waitForProcess handle
    case code of
        ExitSuccess -> return ()
        _           -> exitFailure

resetDir :: FilePath -> IO ()
resetDir path = do
    exist <- doesDirectoryExist path
    if exist then removeDirectoryRecursive path else return ()
    createDirectoryIfMissing True path

main :: IO ()
main = do
    resetDir ("benchmark"</>"contraction")
    resetDir ("benchmark"</>"build")
    putStrLn "Benchmarked contractions:"
    (unzip3 -> (incs, funs, regs)) <- concat <$> mapM makeBenchmarks contIds
    T.writeFile ("benchmark"</>"main"<.>"cpp") $ testCode (T.concat incs) (T.concat funs) (T.concat regs)
    createProcessAndExitOnFailure "cmake" ["-DCMAKE_BUILD_TYPE=Release", ".."]
    createProcessAndExitOnFailure "cmake" ["--build", ".", "--config", "Release"]

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

makeBenchmarks :: Int -> IO [(Text,Text,Text)]
makeBenchmarks (tshow -> num) = do
    expr <- sampleSimple
    let exprText = printContraction False expr
        makeBench size = do
            let test = translate (const size) expr
                s = tshow size
                inc = include "h" [text|cont${num}_$s|]
                benchFun = benchmarkFunction num s $ initData (const size) expr
                register = [text|BENCHMARK(cont${num}_${s}_benchmark)->ComputeStatistics("min", min_time);|]
            compile ("benchmark" </> "contraction") (stripEnd [text|cont${num}_$s|]) test
            return (inc,benchFun,register)
    T.putStr [text|$num) $exprText|]
    mapM makeBench sizes
        
benchmarkFunction :: Text -> Text -> Text -> Text
benchmarkFunction num size init = [text|
    void cont${num}_${size}_benchmark(benchmark::State& state) {
        $init
        for (auto _ : state) {
            cont${num}_$size(userData);
        }
        free_data(userData);
    }|]