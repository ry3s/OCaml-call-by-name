%{
open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token EOC
%token LET IN EQ LT 
%token PLUS MINUS
%token TIMES DIV
%token LBRA RBRA SEMI
%token LPAR RPAR COMMA CONS
%token IF THEN ELSE
%token MATCH WITH BAR UNDERSCORE END 
%token FUN REC RARROW AND
%token EOF 

%left EOC
%left COMMA SEMI 
%right CONS
%left PLUS MINUS
%left TIMES DIV


%start main 
%type <Syntax.command> main
%% 

main: 
  | command_expr EOF     { $1 }
  | command_expr EOC     { $1 }
  | EOF                  { CEnd }
;

command_expr:
  | LET pattern_expr EQ expr                   { CLet ($2,$4) }
/*  | LET REC var var EQ expr                    { CRLet ($3, EFun ($4, $6)) }  */
/*  | LET REC var EQ expr                        { CRLet ($3, $5) } */
  | LET REC var argument_expr              { CRLet ($3, $4) } 
/*  | LET REC var var EQ expr AND mutualrec_expr { CMRLet ([($3,$4,$6)] @ ($8)) }  */
  | LET REC var argument_expr AND mutualrec_expr { CMRLet ([($3,$4)] @ ($6)) }
  | expr                                       { CExp ($1) } 
;
mutualrec_expr:
  | var argument_expr AND mutualrec_expr  { [($1,$2)] @ ($4) }
  | var argument_expr                     { [($1,$2)] }
;
argument_expr:
  | var argument_expr      { EFun ($1,$2) }
  | EQ expr            { $2 }
;     
expr:
  | LET pattern_expr EQ expr IN expr  { ELet ($2,$4,$6) }
  | FUN var RARROW expr               { EFun ($2,$4) }
/*  | LET REC var var EQ expr IN expr   { ERLet ($3, EFun ($4,$6),$8) } */
  | LET REC var argument_expr IN expr   { ERLet ($3, $4, $6) }
  | IF expr THEN expr ELSE expr       { EIf ($2,$4,$6) }
  | MATCH expr WITH branch_expr END   { EMatch ($2,$4) }
  | compare_expr                      { $1 }
;
branch_expr:
  | pattern_expr RARROW expr BAR branch_expr { ($1,$3)::($5) }
  | pattern_expr RARROW expr                 { [($1,$3)] }
;
pattern_expr:
  | INT                            { PInt  $1 }
  | BOOL                           { PBool $1 }
  | var                            { PVar  $1 }
  | UNDERSCORE                     { PUnderscore }
  | LPAR tuple_pattern_expr RPAR   { PTuple ($2) } 
  | pattern_expr CONS pattern_expr { PCons ($1,$3) } 
  | LBRA RBRA                      { PNil }
  | LBRA list_pattern_expr RBRA    { $2 }
  | LPAR pattern_expr RPAR         { $2 }
;
tuple_pattern_expr:
  | pattern_expr COMMA pattern_expr       { [$1;$3] }
  | pattern_expr COMMA tuple_pattern_expr { [$1] @ ($3) }
;
list_pattern_expr:
  | pattern_expr                        { PCons ($1, PNil) }
  | pattern_expr SEMI list_pattern_expr { PCons ($1,$3) }
;
compare_expr:
  | compare_expr EQ arith_expr { EBin (OpEq,$1,$3) }
  | compare_expr LT arith_expr { EBin (OpLt,$1,$3) }
  | cons_expr                  { $1 }
;
cons_expr:
  | arith_expr CONS cons_expr{ ECons ($1,$3) } 
  | arith_expr                { $1 }
;
arith_expr:
  | arith_expr PLUS factor_expr  { EBin(OpAdd,$1,$3) }
  | arith_expr MINUS factor_expr { EBin(OpSub,$1,$3) }  
  | factor_expr                  { $1 }
;
factor_expr:
  | factor_expr TIMES app_expr { EBin(OpMul,$1,$3) }
  | factor_expr DIV app_expr   { EBin(OpDiv,$1,$3) }
  | app_expr                   { $1 }
;
app_expr:
  | app_expr atomic_expr { EApp ($1,$2) }
  | atomic_expr          { $1 }
;
atomic_expr:
  | INT                  { EValue (VInt $1) }
  | BOOL                 { EValue (VBool $1)}
  | ID                   { EVar   $1 }
  | LPAR tuple_expr RPAR { ETuple ($2) } 
  | LBRA RBRA            { ENil }
  | LBRA list_expr RBRA  { $2 } 
  | LPAR expr RPAR       { $2 }
;
tuple_expr:
  | expr COMMA expr       { [$1;$3] }
  | expr COMMA tuple_expr { [$1] @ ($3) }
;
list_expr:
  | expr SEMI list_expr { ECons ($1,$3) }
  | expr                { ECons ($1, ENil) }
;
var:
  | ID { $1 }


