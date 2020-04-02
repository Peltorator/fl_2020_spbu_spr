module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent)

import           LLang               (LAst (..), parseExprInBrackets, parseIf, parseWhile, parseAssign,
                                      parseRead, parseWrite, parseSeq, parseAnything, parseL)

import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_parseExprInBrackets :: Assertion
unit_parseExprInBrackets = do
    runParser parseExprInBrackets "(1)" @?= Success "" (Num 1)
    runParser parseExprInBrackets "  \n  \n\n  ( \n\n\n  __kek_1__   \n\n  \n) \n \n " @?= Success " \n \n " (Ident "__kek_1__" )
    assertBool "" $ isFailure (runParser parseExprInBrackets "(abacaba")

unit_parseIf :: Assertion
unit_parseIf = do
    runParser parseIf "if (x==7) { print (8); print (x); } \n \n else {\nprint(2);}" @?= Success "" 
        (If (BinOp Equal (Ident "x") (Num 7)) (Seq [Write (Num 8), Write (Ident "x")]) (Seq [Write (Num 2)]))
    runParser parseIf "if (x==7) { print (8); print (x); } else {}" @?= Success "" 
        (If (BinOp Equal (Ident "x") (Num 7)) (Seq [Write (Num 8), Write (Ident "x")]) (Seq []))
    assertBool "" $ isFailure (runParser parseIf "print (8)")
    assertBool "" $ isFailure (runParser parseIf "if () { print (8); } else { print (9); }")
    assertBool "" $ isFailure (runParser parseIf "if (x == 7) { print (8); }")
    assertBool "" $ isFailure (runParser parseIf "if (x == 7) { print (8); } else")
    assertBool "" $ isFailure (runParser parseIf "if () { print (8); } { print (9); }")

unit_parseWhile :: Assertion
unit_parseWhile = do
    runParser parseWhile "while (x==7) { print (8); print (x); }" @?= Success "" 
        (While (BinOp Equal (Ident "x") (Num 7)) (Seq [Write (Num 8), Write (Ident "x")]))
    runParser parseWhile "while (x==7) {}" @?= Success "" 
        (While (BinOp Equal (Ident "x") (Num 7)) (Seq []))
    assertBool "" $ isFailure (runParser parseWhile "print (8)")
    assertBool "" $ isFailure (runParser parseWhile "while () { print (8); }")

unit_parseAssign :: Assertion
unit_parseAssign = do
    runParser parseAssign "assign _ (0)" @?= Success "" 
        (Assign "_" (Num 0))
    runParser parseAssign "assign   \n \n  lol_1_2_3__ \n    ( 41+67 )" @?= Success "" 
        (Assign "lol_1_2_3__" (BinOp Plus (Num 41) (Num 67)))
    assertBool "" $ isFailure (runParser parseAssign "print (8)")
    assertBool "" $ isFailure (runParser parseAssign "assign 0 (_)")
    assertBool "" $ isFailure (runParser parseAssign "assign 0 _")
    assertBool "" $ isFailure (runParser parseAssign "assign _ 0")

unit_parseRead :: Assertion
unit_parseRead = do
    runParser parseRead "read _" @?= Success "" 
        (Read "_")
    runParser parseRead "read  \n    lol_1_2_3__" @?= Success "" 
        (Read "lol_1_2_3__")
    assertBool "" $ isFailure (runParser parseRead "print (8)")
    assertBool "" $ isFailure (runParser parseRead "read 0")
    assertBool "" $ isFailure (runParser parseRead "read ( _ )")

unit_parseWrite :: Assertion
unit_parseWrite = do
    runParser parseWrite "print (_)" @?= Success "" 
        (Write (Ident "_"))
    runParser parseWrite "print     \n  \n    (lol_1_2_3__+12)" @?= Success "" 
        (Write (BinOp Plus (Ident "lol_1_2_3__") (Num 12)))
    assertBool "" $ isFailure (runParser parseWrite "read (_)")
    assertBool "" $ isFailure (runParser parseWrite "print 0")
    assertBool "" $ isFailure (runParser parseWrite "print x")

unit_parseSeq :: Assertion
unit_parseSeq = do
    runParser parseSeq "{   \n\n\n   print (5);  \n  read x;   read y;     \n }" @?= Success "" 
        (Seq [Write (Num 5), Read "x", Read "y"])
    runParser parseSeq "{print (5);}" @?= Success "" 
        (Seq [Write (Num 5)])
    runParser parseSeq "{ \n \n \n } \n" @?= Success " \n" 
        (Seq [])
    assertBool "" $ isFailure (runParser parseSeq "print (8)")
    assertBool "" $ isFailure (runParser parseSeq "{ print (5) }")
    assertBool "" $ isFailure (runParser parseSeq "{ print (5) print (6); }")
    assertBool "" $ isFailure (runParser parseSeq "{ print (5); print (6) }")


unit_parseL :: Assertion
unit_parseL = do
    runParser parseL 
       "  { \n\n\n   print (5);  \n  read x;   read y;  if (  (x==5||1)  ) { print (2); } else { while (1) { print (3);  \n }; }; } "
       @?= Success "" (Seq [Write (Num 5), Read "x", Read "y",
               If (BinOp Or (BinOp Equal (Ident "x") (Num 5)) (Num 1)) (Seq [Write (Num 2)]) (Seq [While (Num 1) (Seq [Write (Num 3)])])
           ])
    runParser parseL
        "{read x; if (17 + 2 == x) { print (1); } else { print (2); }; read y; read x; }" 
        @?= Success "" (Seq [
                              Read "x",
                              If (BinOp Equal (BinOp Plus (Num 17) (Num 2)) (Ident "x")) (Seq [Write (Num 1)]) (Seq [Write (Num 2)]),
                              Read "y",
                              Read "x"
                            ])
    assertBool "" $ isFailure (runParser parseL "print (8)")
    assertBool "" $ isFailure (runParser parseL "{ write (505); }")
    assertBool "" $ isFailure (runParser parseL "read x")
    assertBool "" $ isFailure (runParser parseL "assign x (2)")
    assertBool "" $ isFailure (runParser parseL "if (_) { print (5); } else { print (4); }")
    assertBool "" $ isFailure (runParser parseL "while (_) { print (5); }")

