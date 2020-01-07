module Language.Javascript.Types where

type ExpressionOpt = Maybe Expression
type StatementOpt = Maybe Statement
type VariableDecl = (Expression, ExpressionOpt)
type ForDecl a = (a, ExpressionOpt, ExpressionOpt)
type ForInDecl = (Expression, Expression)
type ForVarInDecl = (VariableDecl, Expression)

data Expression = This
                | Identifier String
                | Number String
                | VTrue | VFalse
                | VNull
                | Sequence [Expression]
                | ParenSequence [Expression]

                | Dot Expression Expression
                | Accessor Expression Expression
                | Call Expression [Expression]
                | New Expression

                | PostInc Expression
                | PostDec Expression
                | Delete Expression
                | Void Expression
                | Typeof Expression
                | PreInc Expression
                | PreDec Expression
                | Pos Expression
                | Neg Expression
                | Complement Expression
                | Not Expression

                | Multiply Expression Expression
                | Divide Expression Expression
                | ModuleE Expression Expression

                | Addition Expression Expression
                | Subtraction Expression Expression

                | LShift Expression Expression
                | RShift Expression Expression
                | URShift Expression Expression

                | Less Expression Expression
                | Greater Expression Expression
                | LessEq Expression Expression
                | GreaterEq Expression Expression

                | Equal Expression Expression
                | NEqual Expression Expression

                | BAnd Expression Expression
                | BOr Expression Expression
                | BXor Expression Expression

                | And Expression Expression
                | Or Expression Expression

                | Conditional Expression Expression Expression
                | Assignment Expression Expression
                | SumAssignment Expression Expression
                | MinusAssignment Expression Expression
                | MultAssignment Expression Expression
                | DivAssignment Expression Expression
                | ModAssignment Expression Expression
                | LShiftAssignment Expression Expression
                | RShiftAssignment Expression Expression
                | URightAssignment Expression Expression
                | BAndAssignment Expression Expression
                | BXorAssignment Expression Expression
                | BOrAssignment Expression Expression
                deriving (Show, Eq)

data Statement = BlockStatement [Statement]
               | VariableStatement [VariableDecl]
               | EmptyStatement
               | ExpressionStatement Expression
               | IfStatement Expression Statement StatementOpt
               | ForStatement (ForDecl ExpressionOpt) Statement
               | ForVarStatement (ForDecl [VariableDecl]) Statement
               | ForInStatement ForInDecl Statement
               | ForVarInStatement ForVarInDecl Statement
               | WhileStatement Expression Statement
               | ContinueStatement
               | BreakStatement
               | ReturnStatement ExpressionOpt
               | WithStatement Expression Statement
               | FunctionStatement String [Expression] Statement
               deriving (Eq, Show)
