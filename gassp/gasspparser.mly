/* Ocamlyacc parser for GA$$P*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN DOT LBRACKET RBRACKET MOD INCR DECR BREAK CONTINUE SWITCH CASE DEFAULT COLON EXP
%token NOT EQ NEQ LT LEQ GT GEQ AND OR BNOT BOR BXOR BAND LSHIFT RSHIFT
%token RETURN IF ELSE FOR WHILE INT BOOL CHAR FLOAT VOID STRING
%token <int> LITERAL
%token <bool> BLIT
%token <string> SLIT
%token <char> CHLIT
%token <string> ID FLIT
%token <string> CLASS
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }
 /* | decls cdecl { (fst $1, ($2 :: snd $1)) } */

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { ftyp = $1;
	 fname = $2;
	 fformals = List.rev $4;
	 flocals = List.rev $7;
	 fbody = List.rev $8 } }

/* class body */
cbody:
   mdecl_list  { (List.rev (fst $1) , List.rev (snd $1))} /* fst is methods list snd is variables list*/

/* member declaration list for class members and methods*/
mdecl_list:
  /* nothing */ {([], [])}
  | mdecl_list method_decl{(($2 :: fst $1), snd $1)}
  | mdecl_list local_decl {(fst $1, ($2 :: snd $1))}

local_decl:
  | typ ID ASSIGN expr SEMI{ ($2, $1, $4) } /* member variable with value*/
  | arr_decl ID SEMI  {($2, $1, Noexpr)}/* member arr variable without value*/


/* member declaration members and methods*/
method_decl:
  | typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE { { mtyp = $1;
	                                                                         mname = $2;
	                                                                         mformals = List.rev $4;
	                                                                         mlocals = List.rev $7;
                                                                        	 mbody = List.rev $8 } 
                                                                           }


cdecl: 
    /* CLASS ID LBRACE cbody RBRACE {{cname = $2; clocals = snd $4; cbody = fst $4}} */
    CLASS ID LBRACE cbody RBRACE {()}

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }
arr_decl:
    typ LBRACKET LITERAL RBRACKET {$1}

typ:
    INT   { Int   }
  | CHAR  { Char  }
  | STRING{ String}
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }


vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }
   | typ ID ASSIGN expr SEMI {($1, $2)}
   | arr_decl ID SEMI  {($1, $2)}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | BREAK SEMI                                  {Break}
  | CONTINUE SEMI                               {Continue}
  | SWITCH LPAREN expr RPAREN LBRACE case_stmt_list RBRACE {Switch($3, $6)}

case_stmt_list:
    | case_stmt_list_opt case_stmt_default_opt {List.rev ($2::$1)}

case_stmt_list_opt:
    /* nothing */ {[]}
    | case_stmt_list_opt case_stmt {$2::$1}

case_stmt_default_opt:
    | case_stmt_default {$1}

case_stmt_default:
    DEFAULT expr SEMI {Expr(Default($2))}

case_stmt:
    CASE LPAREN expr RPAREN expr SEMI {Expr(Case($3, $5))}
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | CHLIT            { Chliteral($1)              }
  | SLIT             { Sliteral($1)               }
  | FLIT	           { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr BAND    expr { Binop($1, Band,   $3)   }
  | expr BOR     expr { Binop($1, Bor,    $3)   }
  | expr BXOR     expr { Binop($1, Bxor,    $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr LSHIFT expr { Binop($1, Lshift,   $3)   }
  | expr RSHIFT expr { Binop($1, Rshift,    $3)   }
  | ID DOT ID  LPAREN args_list RPAREN { Mcall($1, $3, $5)}
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | expr INCR     { Unop(Incr, $1)      }
  | expr DECR     { Unop(Decr, $1)      }
  /* | ID INCR          { Unop(Incr, $1)      }
  | ID DECR          { Unop(Decr, $1)      } */
  | NOT expr         { Unop(Not, $2)          }
  | BNOT expr         { Unop(Bnot, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
lit_expr:
    LITERAL          { Literal($1)            }
  | CHLIT            { Chliteral($1)              }
  | SLIT             { Sliteral($1)               }
  | FLIT	           { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
