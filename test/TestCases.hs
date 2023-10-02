module TestCases where

import Lexer
import Lib
import Parser

data TestCase = TestCase {rawTestCase :: String, lexedTestCase :: [Token], parsedTestCase :: [Expr]}

testCases :: [TestCase]
testCases =
  [ TestCase
      { rawTestCase = "let x = 3; let y = x + 4;",
        lexedTestCase = [LET, IDENT "x", EQU, DIGIT 3, SC, LET, IDENT "y", EQU, IDENT "x", PLUS, DIGIT 4, SC, EOF],
        parsedTestCase =
          [ Let {letVar = "x", letEqual = LInt 3},
            Let {letVar = "y", letEqual = BinOp (VarExpr "x") OpPlus (LInt 4)}
          ]
      },
    TestCase
      { rawTestCase = "fun If (x: bool, y: int, z: int) = if x then y else z;",
        lexedTestCase = [FUN, IDENT "If", LPAREN, IDENT "x", COLON, KBOOL, COMMA, IDENT "y", COLON, KINT, COMMA, IDENT "z", COLON, KINT, RPAREN, EQU, IF, IDENT "x", THEN, IDENT "y", ELSE, IDENT "z", SC, EOF],
        parsedTestCase =
          [ FunDecl
              { funDeclName = "If",
                funDeclArgs = [("x", VarBool), ("y", VarInt), ("z", VarInt)],
                funDeclExpr =
                  Conditional {condBool = VarExpr "x", condIf = VarExpr "y", condElse = VarExpr "z"}
              }
          ]
      },
    TestCase
      { rawTestCase = "fun fst (x:int,y:int) = x;",
        lexedTestCase = [FUN, IDENT "fst", LPAREN, IDENT "x", COLON, KINT, COMMA, IDENT "y", COLON, KINT, RPAREN, EQU, IDENT "x", SC, EOF],
        parsedTestCase = [FunDecl {funDeclName = "fst", funDeclArgs = [("x", VarInt), ("y", VarInt)], funDeclExpr = VarExpr "x"}]
      },
    TestCase
      { rawTestCase = "fun fact (n: int) = if n=0 then 1 else n*fact(n-1);",
        lexedTestCase = [FUN, IDENT "fact", LPAREN, IDENT "n", COLON, KINT, RPAREN, EQU, IF, IDENT "n", EQU, DIGIT 0, THEN, DIGIT 1, ELSE, IDENT "n", ASTERISK, IDENT "fact", LPAREN, IDENT "n", MINUS, DIGIT 1, RPAREN, SC, EOF],
        parsedTestCase =
          [ FunDecl
              { funDeclName = "fact",
                funDeclArgs = [("n", VarInt)],
                funDeclExpr =
                  Conditional
                    { condBool = BinOp (VarExpr "n") OpEq (LInt 0),
                      condIf = LInt 1,
                      condElse =
                        BinOp
                          (VarExpr "n")
                          OpMult
                          ( FunCall
                              { funCallName = "fact",
                                funCallArgs = [BinOp (VarExpr "n") OpMinus (LInt 1)]
                              }
                          )
                    }
              }
          ]
      }
  ]
