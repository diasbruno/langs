module Language.Javascript.ES1.Lexer (Token(..), lexer) where

import Text.Parsec
import Data.Maybe (fromMaybe)
import Data.Char

data Token = TI String | TN String
           | TTrue | TFalse | TNull
           | TDot
           | TBreak | TFor | TNew | TVar
           | TContinue | TFunction | TReturn | TVoid
           | TDelete | TIf | TThis | TWhile
           | TElse | TIn | TTypeof | TWith
           | TCase | TDebugger | TExport | TSuper
           | TCatch | TDefault | TExtends | TSwitch
           | TClass | TDo | TFinally | TThrow
           | TConst | TEnum | TImport | TTry
           | TCpl | TNot
           | TOP | TCP | TOB | TCB | TOC | TCC | TC | TSC | TQ | TDC
           | TAdd | TSub | TMul | TDiv | TMod
           | TInc | TDec
           | TLShift | TRShift | TURShift
           | TLShiftE | TRShiftE | TURShiftE
           | TBAnd | TBOr | TBXor
           | TBAndE | TBOrE | TBXorE
           | TLAnd | TLOr
           | TEqual | TDEqual | TTEqual | TNEqual | TDNEqual
           | TAddE | TSubE | TMulE | TDivE | TModE
           | TGreater | TLess | TGreaterEq | TLessEq
           | TASCI
           | TNL
           | TMComment String | TSComment String
           deriving (Show, Eq, Ord)

reservedKeywords :: [String]
reservedKeywords = [ "true", "false", "null"
                   , "break", "for", "new", "var"
                   , "continue", "function", "return", "void"
                   , "delete", "if", "this", "while"
                   , "else", "in", "typeof", "with"
                   -- future reserved word (pg. 11)
                   , "case", "debugger", "export", "super"
                   , "catch", "default", "extends", "switch"
                   , "class", "do", "finally", "throw"
                   , "const", "enum", "import", "try"
                   ]

fromKeyword :: String -> Token
fromKeyword str =
  case str of
    "true" -> TTrue
    "false" -> TFalse
    "null" -> TNull
    "break" -> TBreak
    "for" -> TFor
    "new" -> TNew
    "var" -> TVar
    "continue" -> TContinue
    "function" -> TFunction
    "return" -> TReturn
    "void" -> TVoid
    "delete" -> TDelete
    "if" -> TIf
    "this" -> TThis
    "while" -> TWhile
    "else" -> TElse
    "in" -> TIn
    "typeof" -> TTypeof
    "with" -> TWith
    "case" -> TCase
    "debugger" -> TDebugger
    "export" -> TExport
    "super" -> TSuper
    "catch" -> TCatch
    "default" -> TDefault
    "extends" -> TExtends
    "switch" -> TSwitch
    "class" -> TClass
    "do" -> TDo
    "finally" -> TFinally
    "throw" -> TThrow
    "const" -> TConst
    "enum" -> TEnum
    "import" -> TImport
    "try" -> TTry
    _ -> error ("keyword not found " <> str)

isInitIdentChar, isIdentChar :: Char -> Bool
isInitIdentChar c = isAlpha c || c == '$' || c == '_'
isIdentChar c = isAlphaNum c || c == '$' || c == '_'

identifier :: String -> (Token, String)
identifier str =
  let (i', rest) = span isIdentChar str
  in (tk i', rest)
  where tk str'
          | str' `elem` reservedKeywords = fromKeyword str'
          | otherwise = TI str'

number :: String -> String
number str =
  let p = toNumber
          <$> optionMaybe (many digit)
          <*> optionMaybe ((:[]) <$> char '.')
          <*> optionMaybe (many digit)
          <*> optionMaybe ((:[]) <$> oneOf "eE")
          <*> optionMaybe ((:[]) <$> oneOf "+-")
          <*> optionMaybe (many digit)
  in either (const "") id (runParser p () "<tests>" str)
  where toNumber x y z a b c =
          let tokens' = [x, y, z, a, b, c]
          in fromMaybe "" (foldl (<>) (Just "") tokens')

isOperator :: Char -> Bool
isOperator c =
  c == '=' || c == '>' || c == '<' ||
  c == '!' || c == '+' || c == '-' ||
  c == '&' || c == '|' || c == '/' ||
  c == '~' || c == '%' || c == '^' ||
  c == '*'

equal, dequal, tequal, nequal , dnequal
  , plusEqual, minusEqual
  , divEqual, mulEqual, modEqual
  , lsftequal, rsftequal, ursftequal
  , bandequal, bxorequal, borequal
  , inc , dec
  , add, mul, sub', div', mod'
  , greater, less
  , greaterEqual, lessEqual
  , lsft, rsft, ursft
  , band, bor, bxor
  , land, lor
  , cpl, not'
    :: Parsec String u (Token, Int)

equal        = (TEqual, 1)     <$ char '='
dequal       = (TDEqual, 2)    <$ string "=="
tequal       = (TTEqual, 3)    <$ string "==="
nequal       = (TNEqual, 2)    <$ string "!="
dnequal      = (TDNEqual, 3)   <$ string "!=="
plusEqual    = (TAddE, 2)      <$ string "+="
minusEqual   = (TSubE, 2)      <$ string "-="
mulEqual     = (TMulE, 2)      <$ string "*="
divEqual     = (TDivE, 2)      <$ string "/="
modEqual     = (TModE, 2)      <$ string "%="
greater      = (TGreater, 1)   <$ char '>'
less         = (TLess, 1)      <$ char '<'
greaterEqual = (TGreaterEq, 2) <$ string ">="
lessEqual    = (TLessEq, 2)    <$ string "<="
inc          = (TInc, 2)       <$ string "++"
dec          = (TDec, 2)       <$ string "--"
lsft         = (TLShift, 2)    <$ string "<<"
rsft         = (TRShift, 2)    <$ string ">>"
ursft        = (TURShift, 2)   <$ string ">>>"
lsftequal    = (TLShiftE, 3)    <$ string "<<="
rsftequal    = (TRShiftE, 3)    <$ string ">>="
ursftequal   = (TURShiftE, 4)   <$ string ">>>="
mul          = (TMul, 1)       <$ char '*'
add          = (TAdd, 1)       <$ char '+'
sub'         = (TSub, 1)       <$ char '-'
div'         = (TDiv, 1)       <$ char '/'
mod'         = (TMod, 1)       <$ char '%'
band         = (TBAnd, 1)      <$ char '&'
bxor         = (TBXor, 1)      <$ char '^'
bor          = (TBOr, 1)       <$ char '|'
bandequal    = (TBAndE, 2)      <$ string "&="
bxorequal    = (TBXorE, 2)      <$ string "^="
borequal     = (TBOrE, 2)       <$ string "|="
land         = (TLAnd, 2)      <$ string "&&"
lor          = (TLOr, 2)       <$ string "||"
cpl          = (TCpl, 1)       <$ char '~'
not'         = (TNot, 1)       <$ char '!'

operator :: String -> (Token, String)
operator str =
  let r = runParser p () "<tests>" str
      u (i', len) = (i', drop len str)
  in either (error "parsing operator") u r
  where p = try mulEqual <|>
            try ursftequal <|> try lsftequal <|> try rsftequal <|>
            try divEqual <|> try modEqual <|>
            try ursft <|> try lsft <|> try rsft <|>
            try plusEqual <|> try minusEqual <|>
            try tequal <|> try dequal <|> try equal <|>
            try dnequal <|> try nequal <|>
            try land <|> try lor <|>
            try borequal <|> try bandequal <|> try bxorequal <|>
            try bor <|> try band <|> try bxor <|>
            try inc <|> try dec <|>
            try add <|> try sub'<|> try mul <|> try div' <|> try mod' <|>
            try greaterEqual <|> try lessEqual <|> greater <|> try less <|>
            try cpl <|> try not'

lexChar :: Char -> Token
lexChar c = case c of
              '(' -> TOP
              ')' -> TCP
              '[' -> TOB
              ']' -> TCB
              '{' -> TOC
              '}' -> TCC
              '?' -> TQ
              ':' -> TDC
              '+' -> TAdd
              '-' -> TSub
              '*' -> TMul
              '^' -> TBXor
              '&' -> TBAnd
              '|' -> TBOr
              '/' -> TDiv
              '%' -> TMod
              '~' -> TCpl
              '!' -> TNot
              '=' -> TEqual
              '>' -> TGreater
              '<' -> TLess
              ';' -> TSC
              ',' -> TC
              '\n' -> TNL
              _ | isInitIdentChar c -> TI [c]
                | isDigit c -> TN [c]
                | otherwise -> error "failed to lex char"

lexerP :: (String -> Token) -> Parsec String () (String, Int) -> String -> [Token]
lexerP f p str =
  let r = runParser p () "<tests>" str
      expand (tk, l) = f tk : lexer (drop l str)
  in either (error. show) expand r

lexer :: String -> [Token]
lexer [] = []
lexer [c]
  | isSpace c = []
  | otherwise = [lexChar c]
lexer ('/':'/':str) =
  let p = (\x -> (x, length x)) <$>
        many (satisfy (/= '\n')) <*
        optional endOfLine
  in lexerP TSComment p str
lexer ('/':'*':str) =
  let p = (\x -> (x, length x)) <$>
        many (try asteriskToSpace <|> alphaNum <|> spaces') <*
        string "*/"
  in lexerP TMComment p str
  where spaces' = oneOf " \t\f\v\n"
        asteriskToSpace = char '*' <* notFollowedBy (char '/')
lexer (c:cs)
  | c == '\n' = TNL : lexer cs
  | isSpace c = lexer cs
  | isInitIdentChar c =
    let (what, rest) = identifier (c:cs)
    in what : lexer rest
  | isDigit c =
    lexNumber (c:cs)
  | c == '.' =
    if isDigit (head cs) then
      lexNumber (c:cs)
    else
      TDot : lexer cs
  | isOperator c =
    let (what, rest) = operator (c:cs)
    in what : lexer rest
  | otherwise =
    lexChar c : lexer cs
    where lexNumber str' =
            let n = number str'
            in TN n : lexer (drop (length n) str')
