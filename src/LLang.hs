module LLang where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..))
import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)
import Control.Applicative
import Expr (compute, parseExpr, parseNum, parseIdent, parseOp, parseExactly, parseSpaces, parseSomeSpaces, parseFunctionCall)
import Data.Maybe (fromMaybe)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }
              deriving (Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

type CodeParser = Parser String String LAst

parseExprInBrackets :: Parser String String AST
parseExprInBrackets = do
    parseSpaces
    parseExactly "("
    parseSpaces
    expr <- parseExpr
    parseSpaces
    parseExactly ")"
    parseSpaces
    return expr

parseIf :: CodeParser
parseIf = do
    parseExactly "if"
    cond <- parseExprInBrackets
    parseSpaces
    thn <- parseSeq
    parseSpaces
    parseExactly "else"
    parseSpaces
    els <- parseSeq
    return $ If cond thn els

parseWhile :: CodeParser
parseWhile = do
    parseExactly "while"
    cond <- parseExprInBrackets
    parseSpaces
    body <- parseSeq
    return $ While cond body

parseAssign :: CodeParser
parseAssign = do
    parseExactly "assign"
    parseSomeSpaces
    var <- parseIdent
    expr <- parseExprInBrackets
    return $ Assign var expr

parseRead :: CodeParser
parseRead = do
    parseExactly "read"
    parseSomeSpaces
    var <- parseIdent
    return $ Read var

parseWrite :: CodeParser
parseWrite = do
    parseExactly "print"
    expr <- parseExprInBrackets
    return $ Write expr

parseSeq :: CodeParser
parseSeq = do
    parseExactly "{"
    parseSpaces
    instructions <- many $ parseAnything <* parseSpaces <* parseExactly ";" <* parseSpaces
    parseExactly "}"
    return $ Seq instructions

parseAnything :: CodeParser
parseAnything = parseIf <|> parseWhile <|> parseAssign <|> parseRead <|> parseWrite <|> parseSeq

parseL :: CodeParser
parseL = do
    parseSpaces
    x <- parseSeq
    parseSpaces
    return x

parseArgs :: Parser String String [String]
parseArgs = (fmap (:) parseIdent <*> many (parseSpaces *> parseExactly "," *> parseSpaces *> parseIdent)) <|> pure []

parseDef :: Parser String String Function
parseDef = do
    parseExactly "func"
    parseSpaces
    fu <- parseIdent
    parseSpaces
    parseExactly "("
    parseSpaces
    args <- parseArgs
    parseSpaces
    parseExactly ")"
    parseSpaces
    body <- parseSeq
    parseSpaces
    parseExactly "return"
    expr <- parseExprInBrackets
    parseSpaces
    parseExactly ";"
    return $ Function fu args body expr

parseProg :: Parser String String Program
parseProg = do
    funcs <- (many (parseSpaces *> parseDef))
    body <- parseL
    return $ Program funcs body

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input [] Map.empty

evalExpr :: Configuration -> AST -> Maybe (Configuration, Int)
evalExpr conf (BinOp   op l r) = do
    (c1, evaledL) <- evalExpr conf l
    (c2, evaledR) <- evalExpr c1 r
    return ((c2, compute (BinOp op (Num evaledL) (Num evaledR))))
evalExpr conf (UnaryOp op ast) = do
    (c1, evaled)  <- evalExpr conf ast
    return (c1, compute (UnaryOp op (Num evaled)))
evalExpr conf (Ident str)      = Just (conf, fromMaybe 0 (Map.lookup str (subst conf)))
evalExpr conf (Num num)        = Just (conf, num)
evalExpr conf (FunctionCall fu args') =
    case (foldr evalNext (Just (conf, [])) $ reverse args') of
        Nothing -> Nothing
        Just (c1, revargs'') -> let args'' = reverse revargs'' in do
            func <- Map.lookup fu (defs c1)
            (c2, args''') <- evalFunction (Conf (Map.fromList $ zip (args func) args'') (input c1) (output c1) (defs c1)) func args''
            return (Conf (subst c1) (input c2) (output c2) (defs c1), args''')
  where
  evalNext arg Nothing             = Nothing
  evalNext arg (Just (conf, args)) = case evalExpr conf arg of
    Just (c1, arg') -> Just (c1, arg':args)
    Nothing -> Nothing



eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) conf@(Conf subst input output defs) = do
    ((Conf subst1 input1 output1 defs1), val) <- evalExpr conf cond
    if (val /= 0) then eval thn (Conf subst input1 output1 defs) else eval els (Conf subst input1 output1 defs)

eval (While cond body) conf@(Conf subst input output defs) = do
    ((Conf subst1 input1 output1 defs1), val) <- evalExpr conf cond
    if (val /= 0) 
        then do { step <- eval body (Conf subst input1 output1 defs); eval (While cond body) step; } 
        else return (Conf subst input1 output1 defs)

eval (Assign var expr) conf@(Conf subst input output defs) = do
    ((Conf subst1 input1 output1 defs1), val) <- evalExpr conf expr
    return (Conf (Map.insert var val subst) input1 output1 defs)

eval (Read var) conf@(Conf subst (val:input) output defs) = Just (Conf (Map.insert var val subst) input output defs)
eval (Read var) conf@(Conf subst []          output defs) = Nothing

eval (Write expr) conf@(Conf subst input output defs) = do
    ((Conf subst1 input1 output1 defs1), val) <- evalExpr conf expr
    return (Conf subst input1 (val:output1) defs)

eval (Seq (instr:instrs)) conf@(Conf subst input output defs) = do
    (Conf subst1 input1 output1 defs1) <- eval instr conf
    eval (Seq instrs) (Conf subst1 input1 output1 defs)
eval (Seq []) conf@(Conf subst input output defs) = Just conf


evalFunction :: Configuration -> Function -> [Int] -> Maybe (Configuration, Int)
evalFunction (Conf subst input output defs) (Function name argNames funBody returnExpr) argValues = do
    c1 <- eval funBody (Conf (Map.fromList (zip argNames argValues)) input output defs)
    (Conf subst' input' output' defs') <- eval (Write returnExpr) c1
    return (Conf subst input' (tail output') defs, head output')


instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Eq Program where
  (==) a b = (show a) == (show b)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
