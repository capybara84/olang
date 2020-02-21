
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

type exp =
    | Eof | Unit | Null | WildCard
    | BoolLit of bool | IntLit of int | CharLit of char | FloatLit of float
    | StringLit of string | Ident of ident | IdentMod of ident * expr
    | Record of expr * ident
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
and
    expr = int * exp

type value =
    | VUnit | VNull | VBool of bool | VInt of int | VChar of char | VFloat of float
    | VString of string | VTuple of value list | VCons of value * value
    | VClosure of expr * expr * (value ref) Env.t
    | VBuiltin of (value -> value)


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
    | (_, Eof) -> "<EOF>" | (_, Unit) -> "()" | (_, Null) -> "[]" | (_, WildCard) -> "_"
    | (_, BoolLit b) -> string_of_bool b | (_, IntLit n) -> string_of_int n
    | (_, CharLit c) -> "'" ^ String.make 1 c ^ "'" | (_, FloatLit f) -> string_of_float f
    | (_, StringLit s) -> "\"" ^ s ^ "\"" | (_, Ident id) -> id
    | (_, IdentMod (id, e)) -> id ^ "." ^ expr_to_string e
    | (_, Record (e, id)) -> expr_to_string e ^ "." ^ id
    | (_, Tuple el) -> "(" ^ tuple_to_string el ^ ")"
    | (_, Binary (op, lhs, rhs)) -> "(" ^ expr_to_string lhs ^ " " ^ string_of_binop op
        ^ " " ^ expr_to_string rhs ^ ")"
    | (_, Unary (op, e)) -> "(" ^ string_of_unop op ^ expr_to_string e ^ ")"
    | (_, Let (id, e)) -> "(let " ^ id ^ " = " ^ expr_to_string e ^ ")"
    | (_, LetRec (id, e)) -> "(let rec " ^ id ^ " = " ^ expr_to_string e ^ ")"
    | (_, Fn (e1, e2)) -> "(fn " ^ expr_to_string e1 ^ " -> " ^ expr_to_string e2 ^ ")"
    | (_, Apply (e1, e2)) -> "(" ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ ")"
    | (_, If (e1, e2, e3)) -> "(if " ^ expr_to_string e1 ^ " then " ^ expr_to_string e2
            ^ " else " ^ expr_to_string e3 ^ ")"
    | (_, Comp el) -> "{" ^ comp_to_string el ^ "}"
(*
    | (_, Match (e, lst)) -> 
    | (_, TypeDef _) ->
*)
    | (_, Module name) ->
        "module " ^ name
    | (_, Import (name, Some rename)) ->
        "import " ^ name ^ " as " ^ rename
    | (_, Import (name, None)) ->
        "import " ^ name
and tuple_to_string = function
    | [] -> ""
    | x::[] -> expr_to_string x
    | x::xs -> expr_to_string x ^ ", " ^ tuple_to_string xs
and comp_to_string = function
    | [] -> ""
    | x::xs -> expr_to_string x ^ "; " ^ comp_to_string xs

let rec value_to_string = function
    | VUnit -> "()"
    | VNull -> "[]"
    | VBool b -> string_of_bool b
    | VInt n -> string_of_int n
    | VChar c -> String.make 1 c
    | VFloat f -> string_of_float f
    | VString s -> s
    | VTuple vl -> "(" ^ vlist_to_string vl ^ ")"
    | VCons (_, VCons _) as e -> "[" ^ vcons_to_string e ^ "]"
    | VCons (x, VNull) -> "[" ^ value_to_string x ^ "]"
    | VCons (x, xs) -> value_to_string x ^ ":" ^ value_to_string xs
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"
and vlist_to_string = function
    | [] -> ""
    | x::[] -> value_to_string x
    | x::xs -> value_to_string x ^ ", " ^ vlist_to_string xs
and vcons_to_string = function
    | VCons (lhs, rhs) ->
        begin match rhs with
        | VNull -> value_to_string lhs
        | VCons _ -> value_to_string lhs ^ ", " ^ vcons_to_string rhs
        | _ -> failwith "cons rhs bug"
        end
    | _ -> failwith "cons bug"

