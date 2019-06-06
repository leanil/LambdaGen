{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

import Compile
import Generate.Contraction
import Generate.ContractionText
import Test.FunctionalTest
import Utility
import Control.Comonad (extract)
import Control.Monad (foldM, forM)
import Data.Functor.Foldable (cata)
import Data.Text (Text, concat, pack, stripEnd, unpack)
import qualified Data.Text.IO as T (putStr, putStrLn, writeFile)
import NeatInterpolation
import Prelude hiding (concat)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (FilePath, (</>),(<.>))
import System.IO (print)
import System.Process (cwd, createProcess, proc, waitForProcess)

contractionTestCount :: Int
contractionTestCount = 10
contIds :: [Int]
contIds = [1..contractionTestCount]

evalIds :: [Text]
evalIds = map ((\n -> stripEnd [text|evaluator$n|]) . tshow) [1..(length funcTests)]

createProcessAndExitOnFailure :: String -> [String] -> IO ()
createProcessAndExitOnFailure processName args = do
    (_, _, _, handle) <- createProcess (proc processName args){ cwd = Just "test/build" }
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
    resetDir ("test"</>"kernel")
    resetDir ("test"</>"contraction")
    resetDir ("test"</>"build")
    foldM (\_ (evalId,(test,_)) -> (compile ("test"</>"kernel") evalId test)) Nothing (zip evalIds funcTests) -- TODO: can we fold these whithout a seed?
    putStrLn "Random generated contractions:"
    forM contIds contractionTest
    T.writeFile ("test"</>"main"<.>"cpp") $ 
        testCode (concat $ map (include "h") evalIds ++ map ((\n -> include "hpp" [text|cont${n}test|]) . tshow) contIds)
                 (concat $ zipWith3 evalCase (map tshow [1..]) evalIds (map snd funcTests) ++
                           map (\n -> contCase (n+length funcTests) n) contIds)
    createProcessAndExitOnFailure "cmake" ["-DCMAKE_BUILD_TYPE=Release", ".."]
    createProcessAndExitOnFailure "cmake" ["--build", ".", "--config", "Release", "--parallel"]
    createProcessAndExitOnFailure "ctest" []

testCode :: Text -> Text -> Text
testCode includeText switchText = purge [text|
    $includeText
    #include "util.hpp"
    #include <cstdlib>

    int main(int argc, char** argv) {
        std::map<std::string, double*> bigVectors{
            { "vec", init_data(7,3) },
            { "mat", init_data(1,6) },
            { "a", init_data(1,3) },
            { "b", init_data(1,3) },
            { "mat8", init_data(1,8) },
            { "tens", init_data(1,24) },
            { "M1", init_data(1,6) },
            { "M2", init_data(7,12) }
        };
        switch (atoi(argv[1])) {
        $switchText
        default: return 1;
        }
    }|]

include :: Text -> Text -> Text
include ext evalId = [text|#include "$evalId.$ext"|]

evalCase :: Text -> Text -> Text -> Text
evalCase caseNum evalId expect = [text|case $caseNum: return !check($evalId(bigVectors), "$expect");|]

contCase :: Int -> Int -> Text
contCase (tshow -> caseNum) (tshow -> n) = [text|case $caseNum: return !cont${n}test();|]

contractionTest :: Int -> IO ()
contractionTest (tshow -> num) = do
    expr <- sampleSimple
    let test = translate smallSizes expr
        path = "test" </> "contraction"
        evalName = stripEnd [text|cont$num|]
        exprText = printContraction False expr
    compile path evalName test
    T.putStr [text|$num) $exprText|]
    T.writeFile (path </> (unpack evalName ++ "test") <.> "hpp") $ 
        contractionTestCode num exprText (initData smallSizes expr) (makeEvaluator smallSizes expr)

contractionTestCode :: Text -> Text -> Text -> Text -> Text
contractionTestCode num exprText tensorMap evaluator =
    purge [text|
        //$exprText
        #include "cont$num.h"
        #include "util.hpp"

        bool cont${num}test() {
            $tensorMap
            $evaluator
            return viewEq(cont$num(userData),R);
        }
    |]
