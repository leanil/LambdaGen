{-# LANGUAGE DataKinds #-}

import Compile
import Expr
import FunctionalTest
import Metrics
import Print
import Recursion
import Typecheck
import Control.Comonad (extract)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Text.IO as T (writeFile)
import System.FilePath ((</>))

import Data.Map.Strict
import Control.Monad.State

test :: Expr0
test = fst rSubdiv

main :: IO ()
main = do
    result <- compile (".." </> "test") "eval" test
    case result of
        Just expr -> putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT])) expr
        Nothing -> return ()
