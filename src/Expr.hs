module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', elems, fail',
                              satisfy, success, symbol, symbols)
import           Data.Char   (digitToInt, isDigit, isLetter)
import Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr ((pars, as):parsers) basicParser makeAST makeASTU = (case as of
    Binary NoAssoc    -> do
        left  <- getTail
        prs   <- pars
        right <- getTail
        return (makeAST prs left right)
      <|> getTail
    Binary LeftAssoc  -> do
        (beg, end) <- fmap (,) getTail <*> (many $ fmap (,) pars <*> getTail)
        return (foldl (\le (op, ri) -> makeAST op le ri) beg end)
    Binary RightAssoc  -> do
        (beg, end) <- fmap (,) (many $ fmap (,) getTail <*> pars) <*> getTail
        return (foldr (\(le, op) ri -> makeAST op le ri) end beg)
    Unary             -> fmap makeASTU pars <*> getTail
    ) <|> getTail
 
  where getTail = uberExpr parsers basicParser makeAST makeASTU
        
uberExpr [] x _ _ = x

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
                        (make "||", Binary RightAssoc), (make "&&", Binary RightAssoc), (make "!", Unary),
                        (make "==" <|> make "/=" <|> make "<=" <|> make "<" <|> make ">=" <|> make ">", Binary NoAssoc),
                        (make "+" <|> make "-", Binary LeftAssoc), (make "*" <|> make "/", Binary LeftAssoc),
                        (make "-", Unary), (make "^", Binary RightAssoc)
                     ]
                     (func (fmap Num parseNum) <|> func (fmap Ident parseIdent) <|> func (symbol '(' *> parseExpr <* symbol ')')) BinOp UnaryOp
    where make c = symbols c >>= toOperator
          func f = parseSpaces *> f <* parseSpaces


parseNum :: Parser String String Int
parseNum = fmap (foldl f 0) (some (satisfy isDigit))
  where
    f x y = 10 * x + digitToInt y


-- Парсер для целых чисел
parseNegNum :: Parser String String Int
parseNegNum = fmap (foldl f 0) go
  where
    f x '-' = -x
    f x y   = 10 * x + digitToInt y
    go :: Parser String String String
    go = some (satisfy isDigit) <|> (fmap (\a b -> b ++ a) (many (symbol '-')) <*> some (satisfy isDigit))

parseIdent :: Parser String String String
parseIdent = do
    hd <- (satisfy isLetter) <|> (symbol '_')
    tl <- many $ (satisfy isLetter) <|> (satisfy isDigit) <|> (symbol '_')
    return (hd:tl)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elems ["||", "&&", ">", ">=", "<", "<=", "/=", "==", "+", "-", "*", "/", "^", "!"]  >>= toOperator

parseExactly :: String -> Parser String String String
parseExactly = foldr (\hd tl -> fmap (:) (symbol hd) <*> tl) $ pure ""

parseSpaces :: Parser String String String
parseSpaces = many $ symbol ' ' <|> symbol '\n'


-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "/"  = success Div
toOperator "^"  = success Pow
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator "<=" = success Le
toOperator "<"  = success Lt
toOperator ">=" = success Ge
toOperator ">"  = success Gt
toOperator "&&" = success And
toOperator "||" = success Or
toOperator "!"  = success Not
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing


hlp True = 1
hlp False = 0

compute :: AST -> Int
compute (Num x)            = x
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Pow x y)    = (compute x) ^ (compute y)
compute (BinOp Equal x y)  = hlp ((compute x) == (compute y))
compute (BinOp Nequal x y) = hlp ((compute x) /= (compute y))
compute (BinOp Le x y)     = hlp ((compute x) <= (compute y))
compute (BinOp Lt x y)     = hlp ((compute x) < (compute y))
compute (BinOp Ge x y)     = hlp ((compute x) >= (compute y))
compute (BinOp Gt x y)     = hlp ((compute x) > (compute y))
compute (BinOp And x y)    = (compute x) * (compute y)
compute (BinOp Or x y)     = let frst = compute x in if frst == 0 then compute y else frst
compute (UnaryOp Minus x)  = -(compute x)
compute (UnaryOp Not x)    = if compute x == 0 then 1 else 0
