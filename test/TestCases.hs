module TestCases where

import Common (Type (..))
import Lexer
import Lib
import Parser
import TypeInfer

data TestCase = TestCase {rawTestCase :: String, lexedTestCase :: [Token], parsedTestCase :: [Expr], typedTestCase :: [TypedExpr]}

testCases :: [TestCase]
testCases =
  [ TestCase
      { rawTestCase = "let x = 3; let y = x + 4;",
        lexedTestCase = [LET, IDENT "x", EQU, DIGIT 3, SC, LET, IDENT "y", EQU, IDENT "x", PLUS, DIGIT 4, SC, EOF],
        parsedTestCase =
          [ Let {Parser.letVar = "x", Parser.letEqual = LInt 3},
            Let {Parser.letVar = "y", Parser.letEqual = BinOp (VarExpr "x") OpPlus (LInt 4)}
          ],
        typedTestCase =
          [ LetTExpr {TypeInfer.letVar = Variable TInt "x", TypeInfer.letEqual = IntTExpr 3},
            LetTExpr {TypeInfer.letVar = Variable TInt "y", TypeInfer.letEqual = BinOpTExpr {exprType = TInt, TypeInfer.binOpLeft = IdentTExpr (Variable TInt "x"), TypeInfer.binOp = OpPlus, TypeInfer.binOpRight = IntTExpr 4}}
          ]
      },
    TestCase
      { rawTestCase = "fun If (x: bool, y: int, z: int) = if x then y else z;",
        lexedTestCase = [FUN, IDENT "If", LPAREN, IDENT "x", COLON, KBOOL, COMMA, IDENT "y", COLON, KINT, COMMA, IDENT "z", COLON, KINT, RPAREN, EQU, IF, IDENT "x", THEN, IDENT "y", ELSE, IDENT "z", SC, EOF],
        parsedTestCase =
          [ FunDecl
              { funDeclName = "If",
                funDeclArgs = [("x", TBool), ("y", TInt), ("z", TInt)],
                funDeclExpr =
                  Conditional {Parser.condBool = VarExpr "x", Parser.condIf = VarExpr "y", Parser.condElse = VarExpr "z"}
              }
          ],
        typedTestCase =
          [ FunDeclTExpr
              { funDeclIdent = Variable (TFun [TBool, TInt, TInt] TInt) "If",
                funDeclTArgs = [Variable TBool "x", Variable TInt "y", Variable TInt "z"],
                funDeclTExpr =
                  IfTExpr
                    { exprType = TInt,
                      TypeInfer.condBool = IdentTExpr (Variable TBool "x"),
                      TypeInfer.condIf = IdentTExpr (Variable TInt "y"),
                      TypeInfer.condElse = IdentTExpr (Variable TInt "z")
                    }
              }
          ]
      },
    TestCase
      { rawTestCase = "fun fst (x:int,y:int) = x;",
        lexedTestCase = [FUN, IDENT "fst", LPAREN, IDENT "x", COLON, KINT, COMMA, IDENT "y", COLON, KINT, RPAREN, EQU, IDENT "x", SC, EOF],
        parsedTestCase = [FunDecl {funDeclName = "fst", funDeclArgs = [("x", TInt), ("y", TInt)], funDeclExpr = VarExpr "x"}],
        typedTestCase =
          [ FunDeclTExpr
              { funDeclIdent = Variable (TFun [TInt, TInt] TInt) "fst",
                funDeclTArgs = [Variable TInt "x", Variable TInt "y"],
                funDeclTExpr = IdentTExpr (Variable TInt "x")
              }
          ]
      },
    -- simple funcall
    TestCase
      { rawTestCase = "fun f (x:int) = true;let y = f(1)",
        lexedTestCase = [FUN, IDENT "f", LPAREN, IDENT "x", COLON, KINT, RPAREN, EQU, BOOLEAN True, SC, LET, IDENT "y", EQU, IDENT "f", LPAREN, DIGIT 1, RPAREN, EOF],
        parsedTestCase = [FunDecl {funDeclName = "f", funDeclArgs = [("x", TInt)], funDeclExpr = LBool True}, Let {Parser.letVar = "y", Parser.letEqual = FunCall {funCallName = "f", funCallArgs = [LInt 1]}}],
        typedTestCase = [FunDeclTExpr {funDeclIdent = Variable (TFun [TInt] TBool) "f", funDeclTArgs = [Variable TInt "x"], funDeclTExpr = BoolTExpr True}, LetTExpr {TypeInfer.letVar = Variable TBool "y", TypeInfer.letEqual = FunCallTExpr {funCallIdent = Variable (TFun [TInt] TBool) "f", funCallTArgs = [IntTExpr 1]}}]
      },
    TestCase
      { rawTestCase = "fun fact (n: int) = if n=0 then 1 else n*fact(n-1);",
        lexedTestCase = [FUN, IDENT "fact", LPAREN, IDENT "n", COLON, KINT, RPAREN, EQU, IF, IDENT "n", EQU, DIGIT 0, THEN, DIGIT 1, ELSE, IDENT "n", ASTERISK, IDENT "fact", LPAREN, IDENT "n", MINUS, DIGIT 1, RPAREN, SC, EOF],
        parsedTestCase =
          [ FunDecl
              { funDeclName = "fact",
                funDeclArgs = [("n", TInt)],
                funDeclExpr =
                  Conditional
                    { Parser.condBool = BinOp (VarExpr "n") OpEq (LInt 0),
                      Parser.condIf = LInt 1,
                      Parser.condElse =
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
          ],
        typedTestCase =
          [ FunDeclTExpr
              { funDeclIdent = Variable (TFun [TInt] TInt) "fact",
                funDeclTArgs = [Variable TInt "n"],
                funDeclTExpr =
                  IfTExpr
                    { exprType = TInt,
                      TypeInfer.condBool = BinOpTExpr {exprType = TBool, TypeInfer.binOpLeft = IdentTExpr (Variable TInt "n"), TypeInfer.binOp = OpEq, TypeInfer.binOpRight = IntTExpr 0},
                      TypeInfer.condIf = IntTExpr 1,
                      TypeInfer.condElse =
                        BinOpTExpr
                          { exprType = TInt,
                            TypeInfer.binOpLeft = IdentTExpr (Variable TInt "n"),
                            TypeInfer.binOp = OpMult,
                            TypeInfer.binOpRight =
                              FunCallTExpr
                                { funCallIdent = Variable (TFun [TInt] TInt) "fact",
                                  funCallTArgs =
                                    [ BinOpTExpr
                                        { exprType = TInt,
                                          TypeInfer.binOpLeft = IdentTExpr (Variable TInt "n"),
                                          TypeInfer.binOp = OpMinus,
                                          TypeInfer.binOpRight = IntTExpr 1
                                        }
                                    ]
                                }
                          }
                    }
              }
          ]
      }
  ]
