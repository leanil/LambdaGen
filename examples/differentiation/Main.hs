import Expr
import Replace

expr = diff (add (func "f") (mul (func "g") (func "h")))
            
addPat = pDiff (pAdd (pStar "f") (pStar "g"))
addRep = pAdd (pDiff (pStar "f")) (pDiff (pStar "g"))

mulPat = pDiff (pMul (pStar "f") (pStar "g"))
mulRep = pAdd 
             (pMul (pDiff (pStar "f")) (pStar "g"))
             (pMul (pStar "f") (pDiff (pStar "g")))

main = do
    putStr $ printExpr expr ++ " = \n"
    let expr'  = replaceAll addPat addRep expr
    putStr $ printExpr expr' ++ " = \n"
    let expr'' = replaceAll mulPat mulRep expr'
    putStr $ printExpr expr''