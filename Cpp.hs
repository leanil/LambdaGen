module Cpp where

createEvaluator :: String -> String
createEvaluator body =
    concatMap (\h -> "#include " ++ h ++ "\n") headers ++
    "\n" ++ signature ++ "{\n" ++
    "\treturn " ++ body ++ ";\n}\n"
    where
        headers = ["\"helpers.h\"", "<map>", "<string>", "<vector>"]
        signature = "std::vector<double> evaluator(std::map<std::string, std::vector<double>*> bigVectors)"
