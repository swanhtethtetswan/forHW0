(* Ocamllex scanner for MicroC *)

{ open Gasspparser }

let digit = ['0' - '9']
let digits = digit+
let letter = [ '_' 'a' - 'z' 'A' - 'Z']
let variable = letter['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t' '\r' '\n']+
let float_literal = digit+ '.' digit*
          | digit* '.' digit+
          | digit+ ('.')? digit* ['e' 'E'] ['+' '-']? digit+
          | digit* ('.')? digit+ ['e' 'E'] ['+' '-']? digit+
let ascii = [' '-'!' '#'-'~']
let char_literal = ''' (ascii as chlit) '''
let string_literal = '"' (ascii* as slit) '"'


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS } 
| '-'      { MINUS }
| '*'      { TIMES }
| '%'      { MOD }
| '/'      { DIVIDE }
| "**"     { EXP }
| '.'      { DOT }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "~"      { BNOT }
| "|"      { BOR }
| "^"      { BXOR }
| "&"      { BAND }
|"<<"      { LSHIFT }
|">>"      { RSHIFT }
| "++"     { INCR}
| "--"     { DECR }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "char"   { CHAR }
| "string" { STRING }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "switch" {SWITCH}
| "case" {CASE}
| "default" {DEFAULT}
| "continue" {CONTINUE}
| "break" {BREAK}
| digits as lxm { LITERAL(int_of_string lxm) }
| "class " variable as lxm { CLASS(lxm)}
| char_literal         { CHLIT(chlit) }
| float_literal as lxm { FLIT(lxm) }
| string_literal       { SLIT(slit) }
| variable     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
