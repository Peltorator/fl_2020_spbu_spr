module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..))
import qualified Data.Map as Map
import Control.Applicative
import Expr (evalExpr, parseExpr, parseNum, parseIdent, parseOp, parseExactly, parseSpaces, parseSomeSpaces)
import           Text.Printf (printf)
import           Data.List   (intercalate)
    

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id in

        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      ident = (+1)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

type CodeParser = Parser String String LAst

parseExprInBrackets :: Parser String String AST
parseExprInBrackets = do
    parseSpaces
    parseExactly "("
    parseSpaces
    expr <- parseExpr
    parseSpaces
    parseExactly ")"
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

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []



eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) conf@(Conf subst input output) = do
    val <- evalExpr subst cond
    if (val /= 0) then eval thn conf else eval els conf

eval (While cond body) conf@(Conf subst input output) = do
    val <- evalExpr subst cond
    if (val /= 0) then do { step <- eval body conf; eval (While cond body) step; } else return conf

eval (Assign var expr) conf@(Conf subst input output) = do
    val <- evalExpr subst expr
    return (Conf (Map.insert var val subst) input output)

eval (Read var) conf@(Conf subst (val:input) output) = Just (Conf (Map.insert var val subst) input output)
eval (Read var) conf@(Conf subst []          output) = Nothing

eval (Write expr) conf@(Conf subst input output) = do
    val <- evalExpr subst expr
    return (Conf subst input (val:output))

eval (Seq (instr:instrs)) conf@(Conf subst input output) = do
    newConf <- eval instr conf
    eval (Seq instrs) newConf
eval (Seq []) conf@(Conf subst input output) = Just conf
