{-# LANGUAGE DataKinds, OverloadedStrings #-}

import Compile
import Expr
import Generate.Contraction
import Generate.ContractionText
import Metrics
import Print
import Recursion
import Test.FunctionalTest
import Typecheck
import Utility
import Control.Comonad (extract)
import Control.Monad (replicateM)
import Data.Aeson (encode)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.Text as T (concat)
import Data.Text.IO as T (putStrLn)
import System.FilePath ((</>), (<.>))

test :: Expr0
test = fst rSubdiv

main :: IO ()
main = do
    resetDir "experiment"
    exprs <- replicateM 10 sampleSimple
    saveContEqs ("experiment" </> "contractions" <.> "json") exprs
    exprs' <- loadContEqs ("experiment" </> "contractions" <.> "json")
    T.putStrLn $ T.concat $ map (printContraction False) exprs'
