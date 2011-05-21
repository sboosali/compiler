{
module Parser where

import AST
import Lexer

}

%name parse
%tokentype { Token }
%error { error . show }

%token
    int      { TokenInteger $$ }
    str      { TokenString $$ }
    id       { TokenID $$ }
    type     { TokenType }
    '->'     { TokenArrow }
    array    { TokenArray }
    of       { TokenOf }
    var      { TokenVar }
    function { TokenFunction }
    nil      { TokenNil }
    break    { TokenBreak }
    let      { TokenLet }
    in       { TokenIn }
    end      { TokenEnd }
    '='      { TokenEq }
    ','      { TokenComma }
    ':'      { TokenColon }
    ';'      { TokenSemicolon }
    ':='     { TokenAssign }
    '.'      { TokenDot }
    '{'      { TokenLBrace }
    '}'      { TokenRBrace }
    '('      { TokenLParen }
    ')'      { TokenRParen }
    '['      { TokenLBracket }
    ']'      { TokenRBracket }
    if       { TokenIf }
    then     { TokenThen }
    else     { TokenElse }
    for      { TokenFor }
    while    { TokenWhile }
    do       { TokenDo }
    to       { TokenTo }
    '*'      { TokenTimes }
    '/'      { TokenDivide }
    '+'      { TokenPlus }
    '-'      { TokenMinus }
    '<>'     { TokenNeq }
    '<'      { TokenLT }
    '>'      { TokenGT }
    '<='     { TokenLEQ }
    '>='     { TokenGEQ }
    '&'      { TokenAnd }
    '|'      { TokenOr }


%left '&'
%left '|'
%nonassoc '<>' '<' '>' '<=' '>=' '='
%right of
%nonassoc then else

%right '->' ':='

%left '+' '-'
%left '*' '/'
%nonassoc neg '(' ')'

%right if
%right for while
%nonassoc do to




%%
program :: { Expr }
        : exp { $1 }

decs :: { [Decl] }
     : {- empty -} { [] }
     | dec decs { $1 : $2 }

dec :: { Decl }
    : tydec { $1 }
    | vardec { $1 }
    | fundec { $1 }

tydec :: { Decl }
      : type id '=' ty { TypeDec $2 $4}

ty :: { SyntacticType }
   : id { VarST $1 }
   | '{' '}' { RecordST [] }
   | '{' tyfields '}' { RecordST $2 }
   | array of id { ArrayST $3 }
   | ty '->' ty { FunST [$1] $3}
   | '(' funargs ')' '->' ty { FunST $2 $5 }
   | '(' ')' '->' ty { FunST [] $4 }


funargs :: { [SyntacticType] }
        : ty { [$1] }
	| ty ',' funargs { $1 : $3 }  

tyfields :: { [(ID, TypeID)] }
         : id ':' id { [($1, $3)] }
	 | id ':' id ',' tyfields { ($1, $3) : $5 }

vardec :: { Decl }
       : var id ':=' exp { UntypedVarDec $2 $4 }
       | var id ':' id ':=' exp { TypedVarDec $2 $4 $6 }

fundec :: { Decl }
       : function id '(' ')' '=' exp { UntypedFunDec $2 [] $6 }
       | function id '(' tyfields ')' '=' exp { UntypedFunDec $2 $4 $7 }
       | function id '(' ')' ':' id '=' exp { TypedFunDec $2 [] $6 $8 }
       | function id '(' tyfields ')' ':' id '=' exp { TypedFunDec $2 $4 $7 $9 }

exp :: { Expr }
    : lvalue { $1 }
    | literal { $1 }
    | funcall { $1 }
    | arithmetic { $1 }
    | structures { $1 }
    | assignment { $1 }
    | control { $1 }

{- an lvalue2 is simply a transformer from expressions to expressions -}
lvalue :: { Expr }
       : id lvalue2 { $2 (Identifier $1) } 
lvalue2 :: { Expr -> Expr }
        : {- empty -} { id }
	| '.' id lvalue2 { \object -> ($3 (FieldRef object $2)) }
	| '[' exp ']' lvalue2 { \object -> $4 (ArrayRef object $2) }


literal :: { Expr }
        : nil { Nil }
	| int { Num $1 }
	| str { Str $1 }

funcall :: { Expr }
        : exp '(' ')' { Funcall $1 [] }
	| exp '(' args ')' { Funcall $1 $3}

args :: { [Expr] }
     : exp { [$1] }
     | exp ',' args { $1 : $3 }


arithmetic :: { Expr }
           : exp '+' exp { Binop Add $1 $3 }
           | exp '-' exp { Binop Subtract $1 $3 }
           | exp '*' exp { Binop Multiply $1 $3 }
           | exp '/' exp { Binop Divide $1 $3 }
           | exp '=' exp { Binop Equals $1 $3 }
           | exp '<>' exp { Binop NotEq $1 $3 }
           | exp '<' exp { Binop LessThan $1 $3 }
           | exp '>' exp { Binop GreaterThan $1 $3 }
           | exp '<=' exp { Binop LessEq $1 $3 }
           | exp '>=' exp { Binop GreaterEq $1 $3 }
           | exp '&' exp { Binop And $1 $3 }
           | exp '|' exp { Binop Or $1 $3 }
           | '-' exp %prec neg { Negate $2 }

structures :: { Expr }
           : recordcreation { $1 }
	   | arraycreation { $1 }

recordcreation :: { Expr }
               : id '{' '}' { RecordCreation $1 []}
	       | id '{' recordargs '}' { RecordCreation $1 $3 }

recordargs :: { [(ID, Expr)] }
           : id '=' exp { [($1, $3)] }
	   | id '=' exp ',' recordargs { ($1, $3) : $5 }

arraycreation :: { Expr }
              : id '[' exp ']' of exp { ArrayCreation $1 $3 $6 }

assignment :: { Expr }
           : lvalue ':=' exp { Assign $1 $3 }

control :: { Expr }
        : if exp then exp elsesuffix
              {
                case $5 of
                  Nothing      -> If $2 $4 (Seq [])
                  Just elseexp -> If $2 $4 elseexp
              }
	| while exp do exp { While $2 $4 }
	| for id ':=' exp to exp do exp { For $2 $4 $6 $8 }
	| break { Break }
	| let decs in expseq end { Let $2 $4 } 
      	| '(' expseq ')' { Seq $2 }

elsesuffix :: { Maybe Expr }
           : {- empty -} { Nothing }
           | else exp { Just $2 }

expseq :: { [Expr] }
       : {- empty -} { [] }
       | sequence { $1 }

sequence :: { [Expr] }
         : exp { [$1] }
	 | exp ';' sequence { $1 : $3 }
