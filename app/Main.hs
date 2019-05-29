{-# LANGUAGE DataKinds #-}

import Compile
import Expr
import Generate.Contraction
import Metrics
import Print
import Recursion
import Test.FunctionalTest
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Text.IO as T (writeFile, putStrLn)
import System.FilePath ((</>))

test :: Expr0
test = fst rSubdiv

main :: IO ()
main = do
    let sizes = makeSizes expr
        expr' = calcDims expr
        expr'' = translate expr' sizes
    T.putStrLn $ printContraction False expr'
    putStr $ printExpr (Proxy :: Proxy (R '[])) expr''
    -- result <- compile (".." </> "test") "eval" test
    -- case result of
    --     Just expr -> putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT])) expr
    --     Nothing -> return ()
