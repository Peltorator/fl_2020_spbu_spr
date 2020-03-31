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
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr ((pars, as):parsers) basicParser makeAST = case as of
    NoAssoc    -> do
        left  <- getTail
        prs   <- pars
        right <- getTail
        return (makeAST prs left right)
      <|> getTail
    LeftAssoc  -> do
        (beg, end) <- fmap (,) getTail <*> (many $ fmap (,) pars <*> getTail)
        return (foldl (\le (op, ri) -> makeAST op le ri) beg end)
    RightAssoc  -> do
        (beg, end) <- fmap (,) (many $ fmap (,) getTail <*> pars) <*> getTail
        return (foldr (\(le, op) ri -> makeAST op le ri) end beg)
 
  where getTail = uberExpr parsers basicParser makeAST
        
uberExpr [] x _ = x

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [
                        (make "||", RightAssoc), (make "&&", RightAssoc),
                        (make "==" <|> make "/=" <|> make "<=" <|> make "<" <|> make ">=" <|> make ">", NoAssoc),
                        (make "+" <|> make "-", LeftAssoc), (make "*" <|> make "/", LeftAssoc), (make "^", RightAssoc)
                     ]
                     (fmap Num parseNum <|> fmap Ident parseIdent <|> symbol '(' *> parseExpr <* symbol ')') BinOp
    where make c = symbols c >>= toOperator

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl f 0 `fmap` go
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
parseOp = elems ["||", "&&", ">", ">=", "<", "<=", "/=", "==", "+", "-", "*", "/", "^"]  >>= toOperator

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
