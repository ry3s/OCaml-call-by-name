{
  open MyParser
}
let digit = ['0'-'9']
let bool = "true" | "false"
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| space+      { token lexbuf }
| "let"       { LET }
| "in"        { IN  }
| "="         { EQ }
| "<"         { LT }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIV }
| '('         { LPAR }
| ')'         { RPAR }
| "["         { LBRA }
| "]"         { RBRA }
| ";"         { SEMI }
| ","         { COMMA }
| "::"        { CONS } 
| "if"        { IF   }
| "then"      { THEN }
| "else"      { ELSE }
| "fun"       { FUN  }
| "rec"       { REC  }
| "and"       { AND  }
| "->"        { RARROW }
| ";;"        { EOC }
| "match"     { MATCH }
| "with"      { WITH  }
| "end"       { END   }
| "|"         { BAR }
| "_"         { UNDERSCORE }
| bool   as n { BOOL (bool_of_string n) }
| digit+ as n { INT  (int_of_string n) }
| ident  as n { ID n }
| eof         { EOF  }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
