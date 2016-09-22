import ErrorTest
import Expr
import Type
import Typecheck
import TypePrinter
import Recursion

getError (Right e) = Just e
getError _ = Nothing

main = print $ getError $ cata typecheckAlg typeErrors
