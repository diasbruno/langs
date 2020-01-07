module JavascriptParser where

import Language.Javascript.Types
import Language.Javascript.ES1.Parser
import Test.Tasty
import Test.Tasty.HUnit

--t :: String -> String -> Statement -> Test
t t' e =
  testCase (show t' <> "\n\t" <> show e <> "\n") $
  head (parse t') @?= e
t2 t' e =
  testCase (show t' <> "\n\t" <> show e <> "\n") $
  parse t' @?= e

emptyBlk, emptyReturn :: Statement
emptyBlk = BlockStatement []
emptyReturn = ReturnStatement Nothing
emptyFor :: (ExpressionOpt, ExpressionOpt, ExpressionOpt)
emptyFor = (Nothing, Nothing, Nothing)

id', num' :: String -> Expression
id'= Identifier
num' = Number
mid', mnum' :: String -> ExpressionOpt
mid' = Just . id'
mnum' = Just . num'
var :: String -> ExpressionOpt -> VariableDecl
var x y = (id' x, y)

memberExpression
  , blockStatement
  , variableStatement
  , emptyStatement
  , expressionStatement
  , ifStatement
  , forStatement
  , whileStatement
  , iterationStatement
  , continueStatement
  , breakStatemet
  , returnStatement
  , withStatement
  , functionStatement
  , autoSemiColonInsertion
    :: TestTree

memberExpression =
  testGroup "member expression"
  [ t "a"  (stmt (id' "a"))
  , t "a.b" (stmt (Dot (id' "a") (id' "b")))
  , t "a[1]" (stmt (Accessor (id' "a") (num' "1")))
  , t "a.b[1]" (stmt (Accessor
                      (Dot (id' "a") (id' "b"))
                      (num' "1")))
  , t "a()" (stmt (Call (id' "a") []))
  , t "a(1)" (stmt (Call (id' "a") [num' "1"]))
  , t "a(1, 2)" (stmt (Call (id' "a") [num' "1", num' "2"]))
  , t "a().b" (stmt (Dot (Call (id' "a") []) (id' "b")))
  , t "a()[1]" (stmt (Accessor
                      (Call (id' "a") [])
                      (num' "1")))
  , t "a(1)[1].b" (stmt (Dot
                         (Accessor
                          (Call (id' "a") [num' "1"])
                          (num' "1"))
                        (id' "b")))
  , t "new a" (stmt (New (id' "a")))
  , t "new a()" (stmt (New (Call (id' "a") [])))
  , t "new a(1)" (stmt (New (Call (id' "a") [num' "1"])))
  , t "new a.b(1)" (stmt (New (Call
                               (Dot (id' "a") (id' "b"))
                               [num' "1"])))
  , t "new a(1)[1].b(1)" (stmt (New (Call
                                     (Dot
                                      (Accessor
                                       (Call (id' "a") [num' "1"])
                                       (num' "1"))
                                      (id' "b"))
                                     [num' "1"])))
  ]
  where stmt = ExpressionStatement

blockStatement =
  testGroup "block"
  [ t "{}"  (stmt [])
  , t "{;}" (stmt [EmptyStatement])
  , t "{1}" (stmt [ExpressionStatement (num' "1")])
  , t "{ x; }" (stmt [ExpressionStatement (id' "x")])
  ]
  where stmt = BlockStatement

variableStatement =
  testGroup "variable"
  [ t "var x"
    (stmt [var "x" Nothing])
  , t "var x = 1"
    (stmt [var "x" (mnum' "1")])
  , t "var x, y = 2"
    (stmt [ var "x" Nothing
          , var "y" (mnum' "2")
          ])
  , t "var x = 1, y"
    (stmt [ var "x" (mnum' "1")
          , var "y" Nothing
          ])
  , t "var x = 1, y = 2"
    (stmt [ var "x" (mnum' "1")
          , var "y" (mnum' "2")
          ])
  , t "var x = 1, y = 2, z"
    (stmt [ var "x" (mnum' "1")
          , var "y" (mnum' "2")
          , var "z" Nothing
          ])
  , t "var x = 1, y = 2, z = 3"
    (stmt [ var "x" (mnum' "1")
          , var "y" (mnum' "2")
          , var "z" (mnum' "3")
          ])
  , t "var x = 1, y, z = 3"
    (stmt [ var "x" (mnum' "1")
          , var "y" Nothing
          , var "z" (mnum' "3")
          ])
  , t "var x, y, z = 3"
    (stmt [ var "x" Nothing
          , var "y" Nothing
          , var "z" (mnum' "3")
          ])
  ]
  where stmt = VariableStatement

emptyStatement =
  testGroup "empty"
  [ t ";" EmptyStatement
  ]

expressionStatement =
  testGroup "expression"
  [ t "x" (stmt (id' "x"))
  , t "1" (stmt (num' "1"))
  , t "true" (stmt VTrue)
  , t "false" (stmt VFalse)
  , t "null" (stmt VNull)
  , t "1, 2" (stmt (Sequence [num' "1", num' "2"]))
  , t "(1, 2)" (stmt (ParenSequence [num' "1", num' "2"]))
  , t "++i" (stmt (PreInc (id' "i")))
  , t "--i" (stmt (PreDec (id' "i")))
  , t "typeof x" (stmt (Typeof (id' "x")))
  , t "void x" (stmt (Void (id' "x")))
  , t "delete x" (stmt (Delete (id' "x")))
  , t "x = 1" (stmt (Assignment (id' "x") (num' "1")))
  , t "x = y = 1" (stmt (Assignment
                          (id' "x")
                          (Assignment (id' "y") (num' "1"))))
  , t "x += 1" (stmt (SumAssignment (id' "x") (num' "1")))
  , t "x -= 1" (stmt (MinusAssignment (id' "x") (num' "1")))
  , t "x *= 1" (stmt (MultAssignment (id' "x") (num' "1")))
  , t "x /= 1" (stmt (DivAssignment (id' "x") (num' "1")))
  , t "x %= 1" (stmt (ModAssignment (id' "x") (num' "1")))
  , t "x <<= 1" (stmt (LShiftAssignment (id' "x") (num' "1")))
  , t "x >>= 1" (stmt (RShiftAssignment (id' "x") (num' "1")))
  , t "x >>>= 1" (stmt (URightAssignment (id' "x") (num' "1")))
  , t "x &= 1" (stmt (BAndAssignment (id' "x") (num' "1")))
  , t "x ^= 1" (stmt (BXorAssignment (id' "x") (num' "1")))
  , t "x |= 1" (stmt (BOrAssignment (id' "x") (num' "1")))
  , t "x ? 1 : 2" (stmt (Conditional
                         (id' "x") (num' "1") (num' "2")
                        ))
  , t "x + 1" (stmt (Addition (id' "x") (num' "1")))
  , t "x + y + 1" (stmt (expect Addition))
  , t "x - 1" (stmt (Subtraction (id' "x") (num' "1")))
  , t "x - y - 1" (stmt (expect Subtraction))
  , t "x * 1" (stmt (Multiply (id' "x") (num' "1")))
  , t "x * y * 1" (stmt (expect Multiply))
  , t "x / 1" (stmt (Divide (id' "x") (num' "1")))
  , t "x / y / 1" (stmt (expect Divide))
  , t "x % 1" (stmt (ModuleE (id' "x") (num' "1")))
  , t "x % y % 1" (stmt (expect ModuleE))
  ]
  where stmt = ExpressionStatement
        expect m = (m (m (id' "x") (id' "y")) (num' "1"))

ifStatement =
  testGroup "if"
  [ t "if (1) {}"
    (stmt (num' "1") (BlockStatement []))
  , t "if (1) x; else y;"
    (stmtElse (num' "1")
     (ExpressionStatement (id' "x"))
     (ExpressionStatement (id' "y"))
    )
  , t2 "if (1) { x; } else { y; }"
    [ stmtElse (num' "1")
      (BlockStatement [ExpressionStatement (id' "x")])
      (BlockStatement [ExpressionStatement (id' "y")])
    ]
  ]
  where
    stmt ex st =
      IfStatement ex st Nothing
    stmtElse ex st el =
      IfStatement ex st (Just el)

forStatement =
  testGroup "for"
  [ t "for (;;) {}" (stmtF emptyFor emptyBlk)
  , t "for (var x;;) {}" (ForVarStatement ([var "x" Nothing], Nothing, Nothing) emptyBlk)
  , t "for (x in 1) {}" (ForInStatement (id' "x", num' "1") emptyBlk)
  , t "for (var x in 1) {}" (ForVarInStatement ((id' "x", Nothing), num' "1") emptyBlk)
  ]
  where stmtF = ForStatement

whileStatement =
  testGroup "while"
  [ t "while (1) {}" (WhileStatement (num' "1") emptyBlk)
  , t "while (1) { x; }" (WhileStatement (num' "1")
                          (BlockStatement [ExpressionStatement (id' "x")]))
  ]

iterationStatement =
  testGroup "iteration"
  [ forStatement, whileStatement]

continueStatement =
  testGroup "continue"
  [ t "continue" ContinueStatement
  , t "continue;" ContinueStatement
  , t "continue ;" ContinueStatement
  ]

breakStatemet =
  testGroup "break"
  [ t "break" BreakStatement
  , t "break;" BreakStatement
  , t "break ;" BreakStatement
  ]

returnStatement =
  testGroup "return"
  [ t "return" emptyReturn
  , t "return;" emptyReturn
  , t "return ;" emptyReturn
  , t "return 1;" (ReturnStatement (mnum' "1"))
  ]

withStatement =
  testGroup "with statement"
  [ t "with(1) {}" (stmt (Number "1") [])
  , t "with(1) {\
      \}" (stmt (Number "1") [])
  ]
  where stmt ex blk = WithStatement ex (BlockStatement blk)

functionStatement =
  testGroup "function declaration"
  [ t "function a() {}"
    (stmt "a" [] [])
  , t "function a(x) {}"
    (stmt "a" [id' "x"] [])
  , t "function a(x) {;}"
    (stmt "a" [id' "x"] [EmptyStatement])
  , t "function a(x) { return 1; }"
    (stmt "a" [id' "x"] [ReturnStatement (mnum' "1")])
  , t "function a(x, y) { return 1; }"
    (stmt "a" [id' "x", id' "y"] [ReturnStatement (mnum' "1")])
  , t "function a(x, y) {\
      \\treturn 1;\
      \}\
      \"
    (stmt "a" [id' "x", id' "y"] [ReturnStatement (mnum' "1")])
  ]
  where stmt n ex blk = FunctionStatement n ex (BlockStatement blk)

autoSemiColonInsertion =
  testGroup "auto semi-colon insertion"
  [ t2 "return \na + b"
    [ ReturnStatement Nothing
    , ExpressionStatement (Addition (id' "a") (id' "b"))
    ]
  , t2 "a = b\n++c"
    [ ExpressionStatement (Assignment (id' "a") (id' "b"))
    , ExpressionStatement (PreInc (id' "c"))
    ]
  ]

parserTests :: TestTree
parserTests =
  testGroup "Parser"
  [ memberExpression
  , blockStatement
  , variableStatement
  , emptyStatement
  , expressionStatement
  , ifStatement
  , iterationStatement
  , continueStatement
  , breakStatemet
  , returnStatement
  , withStatement
  , functionStatement
  , autoSemiColonInsertion
  ]
