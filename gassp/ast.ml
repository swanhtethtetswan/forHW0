(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod | Member | Exp | Lshift | Rshift | Band | Bxor | Bor

type uop = Neg | Not | Incr | Decr | Bnot

type typ = Int | Bool | Float | Void | Char | String | Object

type bind = typ * string (*check*)

type expr =
    Literal of int
  | Sliteral of string
  | Chliteral of char
  | Fliteral of string (*check, sting in MC*)
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Mcall of string * string * expr list (* method call needs class name, method name, and args*)
  | Noexpr (*check*)
  | Case of expr * expr
  | Default of expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue
  | Switch of expr * stmt list


type func_decl = {
    ftyp : typ;
    fname : string;
    fformals : bind list;
    flocals : bind list;
    fbody : stmt list;
  }

type method_decl = {
    mtyp : typ;
    mname : string;
    mformals : bind list;
    mlocals : bind list;
    mbody : stmt list;
}

type class_decl = {
    cname : string;
    clocals : bind list;
    cbody : method_decl list;
}

type arr_decl = {
  arrtyp : typ;
  size: int;
}

type program = bind list * func_decl list
(*type program = bind list * func_decl list * class_decl list *)

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"
  | Member -> "."
  | Exp -> "**"
  | Band -> "&"
  | Bor -> "|"
  | Bxor -> "^"
  | Lshift -> "<<"
  | Rshift -> ">>"

let string_of_uop = function
    Neg -> "-"
  | Decr -> "--"
  | Incr -> "++"
  | Not -> "!"
  | Bnot -> "!"

let string_of_char c = String.make 1 c

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Sliteral(s) -> s
  | Chliteral(c) -> string_of_char c
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Mcall(c, m, el) ->
      c ^ "." ^ m ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Case(e1, e2) -> "case(" ^ string_of_expr e1 ^ "): " ^ string_of_expr e2
  | Default(e) -> "default: " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break"
  | Continue -> "continue"
  | Switch(e, stmts) -> "switch(" ^ string_of_expr e ^ ") {\n" ^ String.concat "\n" (List.map string_of_stmt stmts) ^ "}\n" 

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | String -> "string"
  | Object -> "object"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.fformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.flocals) ^
  String.concat "" (List.map string_of_stmt fdecl.fbody) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
