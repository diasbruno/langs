{
module Language.Javascript.ES1.Lexer (Token(..), lexer) where

import Data.Char
}

%wrapper "basic"

$sc = [ \t]

$digit          = 0-9
$alpha          = [a-z A-Z]
$initIdentifier = [$alpha \_ \$]

@decimal    = $digit+
@expoent    = [eE] [\-\+] @decimal

js :-

<0> "break"    { const TBreak    } -- reserved keywords
<0> "for"      { const TFor      }
<0> "new"      { const TNew      }
<0> "var"      { const TVar      }
<0> "continue" { const TContinue }
<0> "function" { const TFunction }
<0> "return"   { const TReturn   }
<0> "void"     { const TVoid     }
<0> "delete"   { const TDelete   }
<0> "if"       { const TIf       }
<0> "this"     { const TThis     }
<0> "while"    { const TWhile    }
<0> "else"     { const TElse     }
<0> "in"       { const TIn       }
<0> "typeof"   { const TTypeof   }
<0> "with"     { const TWith     }

<0> "case"     { const TCase     } -- future reserved word (pg. 11)
<0> "debugger" { const TDebugger }
<0> "export"   { const TExport   }
<0> "super"    { const TSuper    }
<0> "catch"    { const TCatch    }
<0> "default"  { const TDefault  }
<0> "extends"  { const TExtends  }
<0> "switch"   { const TSwitch   }
<0> "class"    { const TClass    }
<0> "do"       { const TDo       }
<0> "finally"  { const TFinally  }
<0> "throw"    { const TThrow    }
<0> "const"    { const TConst    }
<0> "enum"     { const TEnum     }
<0> "import"   { const TImport   }
<0> "try"      { const TTry      }

<0> "true"     { const TTrue     }
<0> "false"    { const TFalse    }
<0> "null"     { const TNull     }

<0> $initIdentifier [$initIdentifier $digit]* { TI }

<0> $digit+ \. $digit* @expoent { TN }
<0> $digit+ { TN }
<0> \. $digit+ @expoent { TN }

<0> $sc+ ;

<0> \n { const TNL }



<0> \; { const TSC }
<0> \, { const TC }
<0> \. { const TDot }

<0> \( { const TOP }
<0> \) { const TCP }
<0> \[ { const TOB }
<0> \] { const TCB }
<0> \{ { const TOC }
<0> \} { const TCC }

<0> "==="  { const TTEqual }
<0> "=="   { const TDEqual }
<0> "!=="  { const TDNEqual }
<0> "!="   { const TNEqual }
<0> "+="   { const TAddE }
<0> "-="   { const TSubE }
<0> "*="   { const TMulE }
<0> "/="   { const TDivE }
<0> "%="   { const TModE }
<0> ">="   { const TGreaterEq }
<0> "<="   { const TLessEq }
<0> "++"   { const TInc }
<0> "--"   { const TDec }
<0> ">>>=" { const TURShiftE }
<0> "<<="  { const TLShiftE }
<0> ">>="  { const TRShiftE }
<0> ">>>"  { const TURShift }
<0> "<<"   { const TLShift }
<0> ">>"   { const TRShift }
<0> "&="   { const TBAndE }
<0> "^="   { const TBXorE }
<0> "|="   { const TBOrE }
<0> "&&"   { const TLAnd }
<0> "||"   { const TLOr }
<0> \=     { const TEqual }
<0> \>     { const TGreater }
<0> \<     { const TLess }
<0> \*     { const TMul }
<0> \+     { const TAdd }
<0> \-     { const TSub }
<0> \/     { const TDiv }
<0> \%     { const TMod }
<0> \&     { const TBAnd }
<0> \^     { const TBXor }
<0> \|     { const TBOr }
<0> \~     { const TCpl }
<0> \!     { const TNot }
<0> \?     { const TQ }
<0> \:     { const TDC }

{

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
           | TSP | TNL
           deriving (Show, Eq, Ord)

lexer = alexScanTokens

}
