{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

import Compile
import Generate.Contraction
import Generate.ContractionText
import Test.FunctionalTest
import Test.ContractionTest
import Utility
import Control.Comonad (extract)
import Control.Monad (foldM_, forM_, replicateM)
import Data.Functor.Foldable (cata)
import Data.Text (Text, concat, pack, stripEnd, unpack)
import qualified Data.Text.IO as T (putStr, putStrLn, writeFile)
import NeatInterpolation
import Prelude hiding (concat)
import System.FilePath ((</>),(<.>))
import System.IO (print)

getContractions :: IO [ContEq]
getContractions = do
    simple <- replicateM 10 $ sample (GenConfig 1 4 1 4 1 6 1 2)
    general <- replicateM 10 $ sample (GenConfig 1 4 1 5 1 20 1 5)
    return $ contTests ++ simple ++ general

evalIds :: [Text]
evalIds = map ((\n -> stripEnd [text|evaluator$n|]) . tshow) [1..(length funcTests)]

main :: IO ()
main = do
    resetDir ("test"</>"kernel")
    resetDir ("test"</>"contraction")
    resetDir ("test"</>"build")
    foldM_ (\_ (evalId,(test,_)) -> compile ("test"</>"kernel") evalId test) Nothing (zip evalIds funcTests) -- TODO: can we fold these whithout a seed?
    putStrLn "Tested contractions:"
    contractions <- getContractions
    let contIds = [1..length contractions]
    forM_ (zip contractions contIds) (uncurry contractionTest)
    T.writeFile ("test"</>"main"<.>"cpp") $ 
        testCode (concat $ map (include "h") evalIds ++ map ((\n -> include "hpp" [text|cont${n}test|]) . tshow) contIds)
                 (concat $ zipWith3 evalCase (map tshow [1..]) evalIds (map snd funcTests) ++
                           map (\n -> contCase (n+length funcTests) n) contIds)
    let runProc = createProcessAndExitOnFailure $ "test" </> "build"
    runProc "cmake" ["-DCMAKE_BUILD_TYPE=Release", ".."]
    runProc "cmake" ["--build", ".", "--config", "Release", "--parallel"]
    runProc "ctest" []

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

contractionTest :: ContEq -> Int -> IO ()
contractionTest expr (tshow -> num) = do
    let test = translate smallSizes expr
        path = "test" </> "contraction"
        evalName = stripEnd [text|cont$num|]
        exprText = printContraction False expr
    compile path evalName test
    T.putStr [text|$num) $exprText|]
    T.writeFile (path </> (unpack evalName ++ "test") <.> "hpp") $ 
        contractionTestCode num exprText (initData smallSizes expr) (loopEvaluator smallSizes expr)

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
