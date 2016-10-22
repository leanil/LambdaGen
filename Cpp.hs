module Cpp where

createEvaluator :: String -> String
createEvaluator body =
    concatMap (\h -> "#include " ++ h ++ "\n") headers ++
    "\n" ++ signature ++ "{\n" ++
    "\treturn " ++ body ++ ";\n}\n"
    where
        headers = ["\"helper.h\"", "\"View.h\"", "<map>", "<string>", "<vector>"]
        signature = "auto evaluator(std::map<std::string, double*> bigVectors)"
