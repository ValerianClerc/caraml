module TestCases where

import           Lexer
import           Lib
import           Parser

data TestCase = TestCase {rawTestCase :: String, lexedTestCase :: [Token], parsedTestCase :: [Expr]}

testCases :: [TestCase]
testCases = [
  TestCase {
      rawTestCase = "let x = 3 in x + 4;",
      lexedTestCase = [LET, IDENT "x", EQU, DIGIT 3, IN, IDENT "x", PLUS, DIGIT 4, SC, EOF],
      parsedTestCase = [Let { letVar = "x", letEqual = LInt 3, letIn = BinOp (VarExpr "x") OpPlus (LInt 3)}]
      },
  TestCase {
      rawTestCase = "fun If (x,y,z) = if x then y else z;",
      lexedTestCase = [FUN, IDENT "If", LPAREN, IDENT "x", COMMA, IDENT "y", COMMA, IDENT "z", RPAREN, EQU, IF, IDENT "x", THEN, IDENT "y", ELSE, IDENT "z", SC, EOF],
      parsedTestCase = [FunDecl { funDeclName = "If", funDeclArgs = ["x", "y", "z"], funDeclExpr =
                                   Conditional { condBool = VarExpr "x", condIf = VarExpr "y", condElse = VarExpr "z"}
                                   }]
      },
  TestCase {
      rawTestCase = "fun fst (x,y) = x;",
      lexedTestCase = [FUN, IDENT "fst", LPAREN, IDENT "x", COMMA, IDENT "y", RPAREN, EQU, IDENT "x", SC, EOF],
      parsedTestCase = [FunDecl { funDeclName = "fst", funDeclArgs = ["x", "y"], funDeclExpr = VarExpr "x" }]
      },
  TestCase {
      rawTestCase = "fun fact (n) = if n=0 then 1 else n*fact(n-1);",
      lexedTestCase = [FUN, IDENT "fact", LPAREN, IDENT "n", RPAREN, EQU, IF, IDENT "n", EQU, DIGIT 0, THEN, DIGIT 1, ELSE, IDENT "n", ASTERISK, IDENT "fact", LPAREN, IDENT "n", MINUS, DIGIT 1, RPAREN, SC, EOF],
      parsedTestCase = [FunDecl { funDeclName = "fact", funDeclArgs = ["n"], funDeclExpr =
                                   Conditional { condBool = BinOp (VarExpr "n") OpEq (LInt 0), condIf = LInt 1, condElse =
                                                   BinOp (VarExpr "n") OpMult (
                                                   FunCall { funCallName = "fact",
                                                             funCallArgs = [BinOp (VarExpr "n") OpMinus (LInt 1)]}
                                                   )}
                               }]
      }
  ]
