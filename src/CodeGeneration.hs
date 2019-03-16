{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, ViewPatterns #-}

module CodeGeneration where

import Expr
import Parallel
import Recursion
import StorageInliner
import Type
import Typecheck
import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Foldable (Fix(..))
import Data.List (intercalate)
import Data.Vinyl

newtype CodeGenT = CodeGenT [String] deriving Show
-- codeGenAlg :: (Result ∈ fields, ResultPack ∈ fields, ParData ∈ fields, TypecheckT ∈ fields) =>
--     RAlgebra (Cofree ExprF (R fields)) CodeGenT

-- codeGenAlg ((getName -> "result") ::< Scalar s) = CodeGenT ["result=" ++ show s]
-- codeGenAlg (r ::< Scalar s) = mkRoot r (getName r ++ "=" ++ show s) "" []

-- codeGenAlg (_ ::< VectorView{}) = CodeGenT [""]

-- codeGenAlg (r ::< Addition (ra :< _,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
--     mkRoot r code code (bs ++ as) where
--         code = withNL a ++ withNL b ++ getName r ++ "=" ++ getName ra ++ "+" ++ getName rb

-- codeGenAlg (r ::< Multiplication (ra :< _,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
--     mkRoot r code code (bs ++ as) where
--         code = withNL a ++ withNL b ++ getName r ++ "=" ++ getName ra ++ "*" ++ getName rb

-- codeGenAlg (r ::< Apply (_,CodeGenT (a:as)) v) =
--     mkRoot r code code (concatMap (\(_,CodeGenT (_:bs)) -> bs) v ++ as) where
--         code = decorate r $ concatMap (\(_,CodeGenT (b:_)) -> withNL b) v ++ a ++
--                             concatMap (\(rb :< _,_) -> "(" ++ getName rb ++ ")") v
--         decorate (getType -> Fix Arrow{}) s = s
--         decorate _                        s = s ++ "(" ++ getName r ++ ")"

-- codeGenAlg (r ::< Let n (ra :< _,CodeGenT (a:as)) (_ :< _,CodeGenT (b:bs))) =
--     mkRoot r code code (bs ++ as) where
--         code = withNL a ++ "auto " ++ n ++ "=" ++ getName ra ++ ";\n" ++ b

-- codeGenAlg (r ::< Lambda v (_,CodeGenT (a:as))) =
--      CodeGenT $ mkLambda v : as where
--         mkLambda ((n,_):xs) = "[=](auto " ++ n ++ "){return\n" ++ mkLambda xs ++ ";}"
--         mkLambda [] = "[=](auto result" ++ threadId r ++ "){\n" ++ a ++ ";}"
--         threadId (snd . getParData -> Just x)
--             | x == 1 = ""
--             | x > 1  = ", unsigned thread_id"
--         threadId (snd . getParData -> Nothing) = ""

-- -- codeGenAlg (r@(getType -> (Fix (Arrow b _))) ::< Lambda v (_,CodeGenT (a:as))) =
-- --     CodeGenT $ ("[=](" ++ params ++ "){return\n" ++ a ++ ";}") : as where
-- --         params = concatMap (\x -> "auto " ++ fst x ++ ", ") v ++ "auto result" ++ threadId r
-- --         threadId (snd . getParData -> Just x)
-- --             | x == 1 = ""
-- --             | x > 1  = ", unsigned thread_id"
-- --         threadId (snd . getParData -> Nothing) = ""

-- codeGenAlg ((getName -> "result") ::< Variable i _) = CodeGenT ["result=" ++ i]
-- codeGenAlg (_ ::< Variable{}) = CodeGenT [""]

-- codeGenAlg (r ::< Map (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
--     mkPar r (withNL b) "Map" (a ++ "," ++ getName rb ++ "," ++ getName r) (bs ++ as)


-- codeGenAlg (r ::< Reduce (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs))) =
--     mkPar r (withNL b) "Reduce" (a ++ "," ++ getName rb ++ "," ++ getName r ++ mkTemp r) (bs ++ as) where
--         mkTemp (fieldVal -> Red (_,Prealloc x)) = ",v_" ++ show x
--         mkTemp _                                          = ""

-- codeGenAlg (r ::< ZipWith (_,CodeGenT (a:as)) (rb :< _,CodeGenT (b:bs)) (rc :< _,CodeGenT (c:cs))) =
--     mkPar r (withNL b ++ withNL c) "Zip" (a ++ "," ++ intercalate "," (map getName [rb, rc, r])) (cs ++ bs ++ as)

-- codeGenAlg (r ::< Compose (_,CodeGenT (a:as)) (_,CodeGenT (b:bs))) = CodeGenT $
--     ("compose(" ++ a ++ "," ++ b ++ "," ++ getName r ++ ")") : as ++ bs

-- mkPar :: (ParData ∈ fields, ResultPack ∈ fields) => R fields -> String -> String -> String -> [String] -> CodeGenT
-- mkPar r@(snd . getParData -> Just t) v hof body groups =
--     CodeGenT $ "" : (if hof == "Reduce" then [mkCommandGroup r ("ParReduceJoin(" ++ body ++ "," ++ show t ++ ");")] else []) ++
--                [(v ++ mkCommandGroup r ("Par" ++ hof ++ "(" ++ body ++ "," ++ show t ++ ");"))] ++ groups
-- mkPar (snd . getParData -> Nothing) v hof body groups =
--     CodeGenT $ (v ++ hof ++ "(" ++ body ++ ")") : groups

-- mkRoot :: (ParData ∈ fields, ResultPack ∈ fields) => R fields -> String -> String -> [String] -> CodeGenT
-- mkRoot r@(snd . getParData -> Just 1) code _ groups =
--     CodeGenT $ "" : mkCommandGroup r (wrap code) : groups where
--         wrap c = "act_cgh->single_task<class SingleKernel>([=] () mutable {\n" ++ (indent "\t" (c ++ ";")) ++ "});"
-- mkRoot (snd . getParData -> Nothing) _ code groups = CodeGenT $ code : groups

-- mkCommandGroup :: ResultPack ∈ fields => R fields -> String -> String
-- mkCommandGroup (fieldVal -> ResultPack (stg,bigVec)) s =
--     "deviceQueue.submit([&] (cl::sycl::handler &cgh) {\n" ++
--     "\tact_cgh = &cgh;\n" ++
--     concatMap
--         (\(BigVector i _ mem) -> let (view, st) = viewType mem 1 "accessor" in
--             "\t" ++ view ++ " " ++ i ++ "(" ++ st ++ ",b_" ++ i ++ ".get_access<rw_access>(cgh));\n")
--         bigVec ++
--     concatMap
--         (\(ResultStg i tn mem) -> let (view, st) = viewType mem tn "accessor" in
--             "\t" ++ view ++ " v_" ++ show i ++ "(" ++ st ++ ",b_" ++ show i ++ ".get_access<rw_access>(cgh));\n")
--         stg ++
--     indent "\t" s ++ "});\n"

-- indent :: String -> String -> String
-- indent tabs code = unlines (map (tabs++) (lines code))

-- withNL :: String -> String
-- withNL [] = []
-- withNL s  = s ++ ",\n"

-- getName :: (Result ∈ fields, ParData ∈ fields) => R fields -> String
-- getName (getPrimary . fieldVal -> Inherit) = "result"
-- getName (getPrimary . fieldVal -> Implicit s) = s
-- getName (getPrimary . fieldVal &&& fst . getParData -> (Prealloc x, tn))
--     | tn == 1 = "v_" ++ show x
--     | tn > 1  = "v_" ++ show x ++ "[thread_id]"

-- getCode :: CodeGenT ∈ fields => R fields -> String
-- getCode (fieldVal -> CodeGenT groups) = concat $ reverse groups