{-# LANGUAGE LambdaCase, OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Generate.ContractionText where

import CppTemplateClosureConv
import Generate.Contraction
import Naming
import Recursion
import Type
import Utility
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.Aeson (decode, encode)
import Data.Functor.Foldable (cata, para)
import Data.Map.Strict (Map, singleton, toList, unions)
import Data.Maybe (fromJust)
import Data.Text (Text, append, intercalate, stripEnd)
import Data.Text as T (concat, cons)
import Data.Text.IO as T (writeFile)
import Data.Text.Lazy as T (lines, toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.IO as T (readFile)
import NeatInterpolation
import System.FilePath (FilePath)

printContraction :: Bool -> ContEq -> Text
printContraction tex expr = math [text|R_$shape = $code|] where
    (math,sub',sum) = case tex of
        True -> (\t -> [text|$$$t$$|], \t -> [text|{$t}|], "\\sum\\limits")
        False -> (id, id, "sum")
    shape = sub $ extract expr
    code = para rAlg expr
    sub = sub' . T.concat . map indexName
    rAlg (_ ::< TensorF (tensorName -> name) (sub -> idxs)) = [text|${name}_$idxs|]
    rAlg (_ ::< SumF (indexName -> name) ops) = stripEnd [text|${sum}_$name$ops'|] where
        ops' = T.concat $ map (stripEnd . T.cons ' ' . (\case (_ :< TensorF{},txt) -> txt; (_ :< SumF{},txt) -> [text|($txt)|])) ops

initData :: Extents -> ContEq -> Text
initData sizes expr =
    [text|std::map<std::string, double*> userData {
        $elems
    };|] where
        elems = intercalate ",\n" $ map (uncurry tensorMapElem) (toList $ collectSizes sizes expr)

initViews :: Extents -> ContEq -> Text
initViews sizes expr = T.concat $ map (uncurry $ initView True) (toList $ collectSizes sizes expr) ++ 
                                  [initView False "R" $ map sizes $ extract expr]

collectSizes :: Extents -> ContEq -> Map Text [Int]
collectSizes dims = cata (ignoreAlg alg) where
    alg (TensorF (tensorName -> name) idxs) = singleton name $ map dims idxs
    alg (SumF _ ops) = unions ops

tensorMapElem :: Text -> [Int] -> Text
tensorMapElem name (tshow . product -> size) = stripEnd [text|{ "$name", init_data(1,$size) }|]

initView :: Bool -> Text -> [Int] -> Text
initView input name dims = [text|$viewT $name$init;|] where
    viewT = cppType $ power double dims
    init = if input then [text|(userData.at("$name"))|] else ""

makeEvaluator :: Extents -> ContEq -> Text -- Generate the for loops that calculate the contraction
makeEvaluator sizes expr@(free :< SumF (indexName -> idx) _) = append (initViews sizes expr) loops where
    loops = foldr (\x t -> for (indexName x) (sizes x) t) body free
    body = [text|
        $core
        $result = sum_$idx;|]
    core = para alg expr
    result = index "R" free
    alg (_ ::< TensorF (tensorName -> name) idxs) = index name idxs
    alg (_ ::< SumF idx ops) = append [text|double sum_$i = 0;|] $ for i (sizes idx) body where
        i = indexName idx
        body = [text|
            double prod = 1;
            $core
            sum_$i += prod;|]
        core = T.concat $ map f ops
        f (_ :< TensorF{}, code) = [text|prod *= $code;|]
        f (_ :< SumF (indexName -> idx) _, code) = [text|
            $code
            prod *= sum_$idx;|]
    index name idxs = append name (T.concat $ map (\(indexName -> idx) -> stripEnd [text|[$idx]|]) idxs)

for :: Text -> Int -> Text -> Text
for var (tshow -> range) body =
    [text|
    for(int $var = 0; $var < $range; ++$var) {
        $body
    }|]

saveContEqs :: FilePath -> [ContEq] -> IO ()
saveContEqs path exprs = T.writeFile path $ T.concat $ json : "\n\n" : pretty where
    json = T.toStrict $ decodeUtf8 $ encode exprs
    pretty = map (printContraction False) exprs

loadContEqs :: FilePath -> IO [ContEq]
loadContEqs path = do
    text <- T.readFile path
    return $ fromJust $ decode $ encodeUtf8 $ head $ T.lines text
