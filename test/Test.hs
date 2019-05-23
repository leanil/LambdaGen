{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Compile
import Test.FunctionalTest
import Control.Comonad (extract)
import Control.Monad (foldM)
import Data.Functor.Foldable (cata)
import Data.Text (Text, concat, pack, unpack)
import NeatInterpolation
import Prelude hiding (concat)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>),(<.>))
import System.IO (print)
import System.Process (cwd, createProcess, proc, waitForProcess)

evalIds :: [String]
evalIds = (map (("evaluator"++) . show) [1..(length funcTests)])

createProcessAndExitOnFailure :: String -> [String] -> IO ()
createProcessAndExitOnFailure processName args = do
    (_, _, _, handle) <- createProcess (proc processName args){ cwd = Just "test/build" }
    code <- waitForProcess handle
    case code of
        ExitSuccess -> return ()
        _           -> exitFailure

main :: IO ()
main = do
    foldM (\_ (evalId,(test,_)) -> (compile ("test"</>"kernel") evalId test)) Nothing (zip evalIds funcTests) -- TODO: can we fold these whithout a seed?
    writeFile ("test"</>"main"<.>"cpp") $ unpack $
        testCode (concat $ map (include . pack) evalIds)
                 (concat $ zipWith3 switch (map (pack . show) [1..]) (map pack evalIds) (map snd funcTests))
    createDirectoryIfMissing True $ "test"</>"build"
    createProcessAndExitOnFailure "cmake" ["-DCMAKE_BUILD_TYPE=Release", ".."]
    createProcessAndExitOnFailure "cmake" ["--build", "."]
    createProcessAndExitOnFailure "ctest" []

testCode :: Text -> Text -> Text
testCode includeText switchText =
    [text|
        $includeText
        #include "tester.hpp"
        #include <cstdlib>

        int main(int argc, char** argv) {
            std::map<std::string, double*> bigVectors{
                { "vec", gen_seq(7,3) },
                { "mat", gen_seq(1,6) },
                { "a", gen_seq(1,3) },
                { "b", gen_seq(1,3) },
                { "mat8", gen_seq(1,8) },
                { "tens", gen_seq(1,24) },
                { "M1", gen_seq(1,6) },
                { "M2", gen_seq(7,12) }
            };
            switch (atoi(argv[1])) {
            $switchText
            default: return 1;
            }
        }
    |]

include :: Text -> Text
include evalId = [text|#include "$evalId.h"|]

switch :: Text -> Text -> Text -> Text
switch caseNum evalId expect = [text|case $caseNum: return check($evalId(bigVectors), "$expect");|]
