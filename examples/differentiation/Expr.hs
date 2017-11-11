{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Functor.Foldable

data ExprF a
    = Func String
    | Add a a
    | Mul a a
    | Diff a
    deriving Functor

type Expr = Fix ExprF

func s  = Fix $ Func s
add a b = Fix $ Add a b
mul a b = Fix $ Mul a b
diff :: Expr -> Expr
diff a  = Fix $ Diff a

printExpr :: Expr -> String
printExpr = cata printAlg where
    printAlg (Func s) = s
    printAlg (Add a b) = "(" ++ a ++ " + " ++ b ++ ")"
    printAlg (Mul a b) = "(" ++ a ++ " * " ++ b ++ ")"
    printAlg (Diff a) = "d" ++ a