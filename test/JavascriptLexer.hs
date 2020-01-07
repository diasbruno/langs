module JavascriptLexer where

import Language.Javascript.ES1.Lexer
import Test.Tasty
import Test.Tasty.HUnit

-- t :: String -> String -> Token -> Test
t t' e = testCase (show t' <> "\n\t" <> show e) $ head (lexer t') @?= e

keywords
  , identifier
  , numbers
  , operators
  , comments
    :: TestTree

keywords =
  testGroup "keywords"
  [ t "for" TFor
  , t "while" TWhile
  , t "try" TTry
  , t "catch" TCatch
  , t "new" TNew
  ]

identifier =
  testGroup "identifier"
  [ t "asdf" (TI "asdf")
  ]

numbers =
  testGroup "numbers"
  [ t "0"     (TN "0")
  , t "0.0"   (TN "0.0")
  , t ".0"    (TN ".0")
  , t "1e1"   (TN "1e1")
  , t "1e+1"  (TN "1e+1")
  , t "1e-1"  (TN "1e-1")
  , t "1.0e1" (TN "1.0e1")
  , t "1.e1"  (TN "1.e1")
  , t ".0e1"  (TN ".0e1")
  ]

operators =
  testGroup "operators"
  [ t "+"   TAdd
  , t "-"   TSub
  , t "/"   TDiv
  , t "*"   TMul
  , t "%"   TMod
  , t "+="  TAddE
  , t "-="  TSubE
  , t "/="   TDivE
  , t "*="   TMulE
  , t "%="   TModE
  , t "="   TEqual
  , t "=="  TDEqual
  , t "===" TTEqual
  , t "~"   TCpl
  , t "!"   TNot
  , t "!="  TNEqual
  , t ">"   TGreater
  , t "<"   TLess
  , t "<="  TLessEq
  , t ">="  TGreaterEq
  , t ">>"  TRShift
  , t "<<"  TLShift
  , t ">>>" TURShift
  , t ">>="  TRShiftE
  , t "<<="  TLShiftE
  , t ">>>=" TURShiftE
  , t "&" TBAnd
  , t "^" TBXor
  , t "|" TBOr
  , t "&=" TBAndE
  , t "^=" TBXorE
  , t "|=" TBOrE
  ]

comments =
  testGroup "comments"
  [ t "//" (TSComment "")
  , t "//comment" (TSComment "comment")
  , t "/**/" (TMComment "")
  , t "/*comment*/" (TMComment "comment")
  , t "/**comment**/" (TMComment "*comment*")
  , t "/*\ncomment\n*/" (TMComment "\ncomment\n")
  ]

lexerTests =
  testGroup "lexer"
  [ identifier
  , keywords
  , numbers
  , operators
  , comments
  ]
