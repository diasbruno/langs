{

module Language.Javascript.ES1.Parser where

import Language.Javascript.Types
import Language.Javascript.ES1.Lexer as L

import Data.Maybe (fromMaybe)
import Data.Char

}

%token
 identifier_name { L.TI $$ }
 number          { L.TN $$ }
 true            { L.TTrue }
 false           { L.TFalse }
 null            { L.TNull }
 dot             { L.TDot }
 break           { L.TBreak }
 for             { L.TFor }
 new             { L.TNew }
 var             { L.TVar }
 continue        { L.TContinue }
 function        { L.TFunction }
 return          { L.TReturn }
 void            { L.TVoid }
 delete          { L.TDelete }
 if              { L.TIf }
 this            { L.TThis }
 while           { L.TWhile }
 else            { L.TElse }
 typeof          { L.TTypeof }
 with            { L.TWith }
 case            { L.TCase }
 open_paren      { L.TOP }
 close_paren     { L.TCP }
 open_bracket    { L.TOB }
 close_bracket   { L.TCB }
 open_curly      { L.TOC }
 close_curly     { L.TCC }
 neg             { L.TSub }
 pos             { L.TAdd }
 cpl             { L.TCpl }
 not             { L.TNot }
 inc             { L.TInc }
 dec             { L.TDec }
 mul             { L.TMul }
 div             { L.TDiv }
 mod             { L.TMod }
 lsft            { L.TLShift }
 rsft            { L.TRShift }
 ursft           { L.TURShift }
 greaterequal    { L.TGreaterEq }
 lessequal       { L.TLessEq }
 greater         { L.TGreater }
 less            { L.TLess }
 dequal          { L.TDEqual }
 nequal          { L.TNEqual }
 equal           { L.TEqual }
 band            { L.TBAnd }
 bxor            { L.TBXor }
 bor             { L.TBOr }
 band_equal      { L.TBAndE }
 bxor_equal      { L.TBXorE }
 bor_equal       { L.TBOrE }
 land            { L.TLAnd }
 lor             { L.TLOr }
 plus_equal      { L.TAddE }
 minus_equal     { L.TSubE }
 div_equal       { L.TDivE }
 mul_equal       { L.TMulE }
 mod_equal       { L.TModE }
 lsft_equal      { L.TLShiftE }
 rsft_equal      { L.TRShiftE }
 ursft_equal     { L.TURShiftE }
 comma           { L.TC }
 semi_colon      { L.TSC }
 q               { L.TQ }
 dc              { L.TDC }
 in              { L.TIn }
 auto            { L.TNL }

%name js
%tokentype { L.Token }

%%

js :: { [Statement] }
 : statement WSP js { $1 : $3 }
 | statement { [$1] }

maybe(p)
 : p { Just $1 }
 |   { Nothing }

WSP
 : maybe(auto) {}

parens(p)
 : open_paren WSP p WSP close_paren { $3 }

bracket(p)
 : open_bracket WSP p WSP close_bracket { $3 }

curly(p)
 : open_curly WSP p WSP close_curly { $3 }

numbers :: { Expression }
 : number { Number $1 }

identifier :: { Expression }
 : identifier_name { Identifier $1 }

literals :: { Expression }
 : numbers { $1 }
 | true { VTrue }
 | false { VFalse }
 | null  { VNull }

primary_expression :: { Expression }
 : this    { This }
 | identifier { $1 }
 | literals { $1 }
 | parens(expression) { toParenExpression $1 }

argument_list :: { [Expression] }
 :                                                   { [] }
 | assignment_expression                             { [$1] }
 | argument_list WSP comma WSP assignment_expression { $1 <> [$5] }

function_call :: { [Expression] }
 : parens(argument_list) { $1 }

member_expression :: { Expression }
 : member_expression dot WSP primary_expression { Dot $1 $4 }
 | member_expression bracket(expression) { Accessor $1 $2 }
 | member_expression function_call { Call $1 $2 }
 | primary_expression { $1 }

new_expression :: { Expression }
 : member_expression { $1 }
 | new WSP new_expression { New $3 }

call_expression :: { Expression }
 : member_expression { $1 }
 | call_expression bracket(expression) { Accessor $1 $2 }
 | call_expression function_call { Call $1 $2 }
 | call_expression dot WSP identifier { Dot $1 $4 }

lhr_expression :: { Expression }
 : new_expression { $1 }
 | call_expression { $1 }

postfix_expression :: { Expression }
 : lhr_expression maybe(auto) { $1 }
 | lhr_expression inc WSP     { PostInc $1 }
 | lhr_expression dec WSP     { PostDec $1 }

unary_expression :: { Expression }
 : postfix_expression { $1 }
 | delete WSP unary_expression { Delete $3 }
 | void   WSP unary_expression { Void $3 }
 | typeof WSP unary_expression { Typeof $3 }
 | inc    WSP unary_expression { PreInc $3 }
 | dec    WSP unary_expression { PreDec $3 }
 | pos    WSP unary_expression { Pos $3 }
 | neg    WSP unary_expression { Neg $3 }
 | cpl    WSP unary_expression { Complement $3 }
 | not    WSP unary_expression { Not $3 }

multiplicative_expression :: { Expression }
 : multiplicative_expression WSP mul WSP unary_expression { Multiply $1 $5 }
 | multiplicative_expression WSP div WSP unary_expression { Divide $1 $5 }
 | multiplicative_expression WSP mod WSP unary_expression { ModuleE $1 $5 }
 | unary_expression WSP { $1 }

additive_expression :: { Expression }
 : additive_expression WSP pos WSP multiplicative_expression { Addition $1 $5 }
 | additive_expression WSP neg WSP multiplicative_expression { Subtraction $1 $5 }
 | multiplicative_expression { $1 }

shift_expression :: { Expression }
 : shift_expression WSP lsft  WSP additive_expression { LShift $1 $5 }
 | shift_expression WSP rsft  WSP additive_expression { RShift $1 $5 }
 | shift_expression WSP ursft WSP additive_expression { URShift $1 $5 }
 | additive_expression { $1 }

relational_expression :: { Expression }
 : relational_expression WSP greater      WSP shift_expression { Greater $1 $5 }
 | relational_expression WSP less         WSP shift_expression { Less $1 $5 }
 | relational_expression WSP lessequal    WSP shift_expression { LessEq $1 $5 }
 | relational_expression WSP greaterequal WSP shift_expression { GreaterEq $1 $5 }
 | shift_expression { $1 }

eq_expression :: { Expression }
 : eq_expression WSP dequal WSP relational_expression { Equal $1 $5 }
 | eq_expression WSP nequal WSP relational_expression { NEqual $1 $5 }
 | relational_expression { $1 }

bit_and_expression :: { Expression }
 : bit_and_expression WSP band WSP eq_expression { BAnd $1 $5 }
 | eq_expression { $1 }

bit_xor_expression :: { Expression }
 : bit_xor_expression WSP bxor WSP bit_and_expression { BXor $1 $5 }
 | bit_and_expression { $1 }

bit_or_expression :: { Expression }
 : bit_or_expression WSP bor WSP bit_xor_expression { BOr $1 $5 }
 | bit_xor_expression { $1 }

logical_and_expression :: { Expression }
 : logical_and_expression WSP land WSP bit_or_expression { And $1 $5 }
 | bit_or_expression { $1 }

logical_or_expression :: { Expression }
 : logical_or_expression WSP lor WSP logical_and_expression { Or $1 $5 }
 | logical_and_expression { $1 }

conditional_expression :: { Expression }
  : logical_or_expression WSP q WSP assignment_expression WSP dc WSP assignment_expression
    { Conditional $1 $5 $9 }
  | logical_or_expression { $1 }

assignment_expression :: { Expression }
 : lhr_expression WSP equal       WSP assignment_expression
    { Assignment $1 $5 }
 | lhr_expression WSP minus_equal WSP assignment_expression
    { MinusAssignment $1 $5 }
 | lhr_expression WSP plus_equal  WSP assignment_expression
    { SumAssignment $1 $5 }
 | lhr_expression WSP mul_equal   WSP assignment_expression
    { MultAssignment $1 $5 }
 | lhr_expression WSP div_equal   WSP assignment_expression
    { DivAssignment $1 $5 }
 | lhr_expression WSP mod_equal   WSP assignment_expression
    { ModAssignment $1 $5 }
 | lhr_expression WSP lsft_equal  WSP assignment_expression
    { LShiftAssignment $1 $5 }
 | lhr_expression WSP rsft_equal  WSP assignment_expression
    { RShiftAssignment $1 $5 }
 | lhr_expression WSP ursft_equal WSP assignment_expression
    { URightAssignment $1 $5 }
 | lhr_expression WSP band_equal  WSP assignment_expression
    { BAndAssignment $1 $5 }
 | lhr_expression WSP bxor_equal  WSP assignment_expression
    { BXorAssignment $1 $5 }
 | lhr_expression WSP bor_equal   WSP assignment_expression
    { BOrAssignment $1 $5 }
 | conditional_expression { $1 }

expression :: { Expression }
 : expression WSP comma WSP assignment_expression { Sequence ( $1 : [$5] ) }
 | assignment_expression WSP { $1 }

statement_list :: { [Statement] }
 : statement statement_list { $1 : $2 }
 | statement WSP { [$1] }

block_statement :: { Statement }
 : curly(statement_list) { BlockStatement $1 }
 | open_curly WSP close_curly { BlockStatement [] }

empty_statement :: { Statement }
 : semi_colon { EmptyStatement }

expression_statement :: { Statement }
 : expression WSP maybe(semi_colon) { ExpressionStatement $1 }

variable_declaration_initializer :: { Expression }
 : equal WSP assignment_expression { $3 }

variable_declaration :: { VariableDecl }
 : identifier WSP maybe(variable_declaration_initializer) { ($1, $3) }

variable_declaration_list :: { [VariableDecl] }
 : variable_declaration                                 { [$1] }
 | variable_declaration WSP comma WSP variable_declaration_list { $1 : $5 }

variable_statement :: { Statement }
 : var WSP variable_declaration_list WSP maybe(semi_colon) { VariableStatement $3 }

if_statement :: { Statement }
 : if WSP parens(expression) WSP statement WSP else WSP statement { IfStatement $3 $5 (Just $9) }
 | if WSP parens(expression) WSP statement WSP { IfStatement $3 $5 Nothing }

for_declaration :: { ForDecl ExpressionOpt }
 : maybe(expression) WSP semi_colon WSP maybe(expression) WSP semi_colon WSP maybe(expression)
   { ($1, $5, $9) }

for_var_declaration :: { ForDecl [VariableDecl] }
 : var WSP variable_declaration_list WSP semi_colon WSP maybe(expression) WSP semi_colon WSP maybe(expression)
   { ($3, $7, $11) }

for_in_declaration :: { ForInDecl }
 : lhr_expression in WSP expression { ($1, $4) }

for_var_in_declaration :: { ForVarInDecl }
 : var WSP variable_declaration WSP in WSP expression { ($3, $7) }

for_statement :: { Statement }
 : for WSP parens(for_var_declaration) WSP statement { ForVarStatement $3 $5 }
 | for WSP parens(for_in_declaration) WSP statement { ForInStatement $3 $5 }
 | for WSP parens(for_var_in_declaration) WSP statement { ForVarInStatement $3 $5 }
 | for WSP parens(for_declaration) WSP statement { ForStatement $3 $5 }

while_statement :: { Statement }
 : while WSP parens(expression) WSP statement { WhileStatement $3 $5 }

iteration_statement :: { Statement }
 : for_statement { $1 }
 | while_statement { $1 }

continue_statement :: { Statement }
 : continue WSP maybe(semi_colon) { ContinueStatement }

break_statement :: { Statement }
 : break WSP maybe(semi_colon) { BreakStatement }

return_statement :: { Statement }
 : return auto { ReturnStatement Nothing }
 | return maybe(expression) WSP maybe(semi_colon) { ReturnStatement $2 }

with_statement :: { Statement }
 : with WSP parens(expression) WSP statement { WithStatement $3 $5 }

formal_params_list
 : identifier WSP comma WSP formal_params_list { $1 : $5 }
 | identifier { [$1] }
 |            { []   }

function_statement :: { Statement }
 : function WSP identifier_name WSP parens(formal_params_list) WSP block_statement
   { FunctionStatement $3 $5 $7 }

statement :: { Statement }
 : block_statement { $1 }
 | variable_statement { $1 }
 | empty_statement { $1 }
 | expression_statement { $1 }
 | if_statement { $1 }
 | iteration_statement { $1 }
 | continue_statement { $1 }
 | break_statement { $1 }
 | return_statement { $1 }
 | with_statement { $1 }
 | function_statement { $1 }

{
toParenExpression (Sequence x) = ParenSequence x

happyError s = error ("Parser error " <> show s)

parse = js . L.lexer
}
