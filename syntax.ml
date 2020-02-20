
exception Error of string

let g_verbose = ref false

type ident = string

type token
    = EOF | NEWLINE | ID of ident | C_ID of ident | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | FLOAT_LIT of float | STRING_LIT of string | TVAR of int
    | MODULE | IMPORT | AS | TYPE | LET | REC | FUN | FN | IF | THEN
    | ELSE | MATCH | WHEN | MUTABLE
    | WILDCARD | PLUS | STAR | PERCENT | COMMA | SEMI | DOT | RANGE
    | MINUS | RARROW | EQ | EQL | NOT | NEQ | LT | LE | LARROW | GT | GE
    | COLON | ASSIGN | OR | LOR | AND | LAND | LSBRA | RSBRA | NULL
    | LPAR | RPAR | UNIT | LBRACE | RBRACE | SLASH

type token_t = {
    token : token;
    line : int;
    col : int;
}

type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod | BinLT | BinLE
        | BinGT | BinGE | BinEql | BinNeq | BinLor | BinLand | BinCons

type unop = UNot | UMinus

type expr =
    | Eof | Unit | Null | WildCard
    | BoolLit of bool | IntLit of int | CharLit of char | FloatLit of float
    | StringLit of string | Ident of ident | IdentMod of ident * expr
    | Tuple of expr list
    | Binary of binop * expr * expr
    | Unary of unop * expr
    | Let of ident * expr
    | LetRec of ident * expr
    | Fn of expr * expr
    | Apply of expr * expr
    | If of expr * expr * expr
    | Comp of expr list
(*
    | Match of expr * (pattern * expr) list
    | TypeDef of int option * ident * typ
*)
    | Module of ident
    | Import of ident * ident option


let token_to_string = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>" | ID id -> id | C_ID id -> id
    | BOOL_LIT b -> string_of_bool b | INT_LIT n -> string_of_int n
    | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'"
    | FLOAT_LIT f -> string_of_float f | STRING_LIT s -> "\"" ^ s ^ "\""
    | TVAR n -> "'" ^ string_of_int n
    | MODULE -> "module" | IMPORT -> "import" | AS -> "as" | TYPE -> "type" 
    | LET -> "let" | REC -> "rec" | FUN -> "fun" | FN -> "fn" | IF -> "if"
    | THEN -> "then" | ELSE -> "else" | MATCH -> "match" | WHEN -> "when"
    | MUTABLE -> "mutable" | WILDCARD -> "_" | PLUS -> "+" | STAR -> "*"
    | PERCENT -> "%" | COMMA -> "," | SEMI -> ";" | DOT -> "." | RANGE -> ".."
    | MINUS -> "-" | RARROW -> "->" | EQ -> "=" | EQL -> "=="
    | NOT -> "!" | NEQ -> "!=" | LT -> "<" | LE -> "<=" | LARROW -> "<-"
    | GT -> ">" | GE -> ">=" | COLON -> ":" | ASSIGN -> ":=" | OR -> "|"
    | LOR -> "||" | AND -> "&" | LAND -> "&&" | LSBRA -> "[" | RSBRA -> "]"
    | NULL -> "[]" | LPAR -> "(" | RPAR -> ")" | UNIT -> "()" | LBRACE -> "{"
    | RBRACE -> "}" | SLASH -> "/"

let string_of_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*"
    | BinDiv -> "/" | BinMod -> "%" | BinLT -> "<"
    | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">="
    | BinEql -> "==" | BinNeq -> "!=" | BinLor -> "||"
    | BinLand -> "&&" | BinCons -> ":"

let string_of_unop = function
    | UNot -> "!"
    | UMinus -> "-"

let rec expr_to_string = function
    | Eof -> "<EOF>" | Unit -> "()" | Null -> "[]" | WildCard -> "_"
    | BoolLit b -> string_of_bool b| IntLit n -> string_of_int n
    | CharLit c -> "'" ^ String.make 1 c ^ "'" | FloatLit f -> string_of_float f
    | StringLit s -> "\"" ^ s ^ "\"" | Ident id -> id
    | IdentMod (id, e) -> id ^ expr_to_string e
    | Tuple el -> "(" ^ tuple_to_string el ^ ")"
    | Binary (op, lhs, rhs) -> "(" ^ expr_to_string lhs ^ " " ^ string_of_binop op
        ^ " " ^ expr_to_string rhs ^ ")"
    | Unary (op, e) -> "(" ^ string_of_unop op ^ expr_to_string e ^ ")"
    | Let (id, e) -> "(let " ^ id ^ " = " ^ expr_to_string e ^ ")"
    | LetRec (id, e) -> "(let rec " ^ id ^ " = " ^ expr_to_string e ^ ")"
    | Fn (e1, e2) -> "(fn " ^ expr_to_string e1 ^ " -> " ^ expr_to_string e2 ^ ")"
    | Apply (e1, e2) -> "(" ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ ")"
    | If (e1, e2, e3) -> "(if " ^ expr_to_string e1 ^ " then " ^ expr_to_string e2 ^ " else "
        ^ expr_to_string e3 ^ ")"
    | Comp el -> "{" ^ comp_to_string el ^ "}"
(*
    | Match (e, lst) -> 
    | TypeDef _ ->
*)
    | Module name ->
        "module " ^ name
    | Import (name, Some rename) ->
        "import " ^ name ^ " as " ^ rename
    | Import (name, None) ->
        "import " ^ name
and tuple_to_string = function
    | [] -> ""
    | x::[] -> expr_to_string x
    | x::xs -> expr_to_string x ^ ", " ^ tuple_to_string xs
and comp_to_string = function
    | [] -> ""
    | x::xs -> expr_to_string x ^ "; " ^ comp_to_string xs

