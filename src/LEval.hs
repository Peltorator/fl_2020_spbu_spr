module LEval where

import LLang (Program (..), Configuration (..))

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program functions main) inp = eval main (Conf Map.empty inp [] (Map.fromList (fmap (\func -> (func, name func))) functions))

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg code inp = case (runParser parseProg code) of
    Success (InputStream stream curPos) result -> if (curPos == length stream) then evalProg result inp else Nothing
    Failure _                                  -> Nothing
