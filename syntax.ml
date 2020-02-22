
exception Error of string

let g_verbose = ref false

type ident = string

type token
    = EOF | NEWLINE | ID of ident | C_ID of ident | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | FLOAT_LIT of float | STRING_LIT of string | TVAR of int
    | MODULE | IMPORT | AS | DECL | TYPE | AND | LET | REC | FUN | FN | IF | THEN
    | ELSE | MATCH | WHEN | MUTABLE | WILDCARD | PLUS | STAR | PERCENT | COMMA
    | SEMI | DOT | RANGE | MINUS | RARROW | EQ | EQL | NOT | NEQ | LT | LE
    | LARROW | GT | GE | COLON | ASSIGN | OR | LOR | AMP | LAND | LSBRA | RSBRA
    | NULL | LPAR | RPAR | UNIT | LBRACE | RBRACE | SLASH

type token_t = {
    token : token;
    line : int;
    col : int;
}


type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod | BinLT | BinLE
        | BinGT | BinGE | BinEql | BinNeq | BinLor | BinLand | BinCons

type unop = UNot | UMinus

type pattern =
    | PatNull | PatWildCard | PatBool of bool | PatInt of int | PatChar of char
    | PatFloat of float | PatString of string
    | PatIdent of ident | PatTuple of pattern list | PatCons of pattern * pattern
    | PatAs of pattern * ident | PatOr of pattern * pattern

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
    | Match of expr * (pattern * expr) list
(*
    | TypeDef of int option * ident * typ
*)
    | Module of ident
    | Import of ident * ident option
and
    expr = int * exp
and typ =
    | Tyvar of int * typ option ref
    | Tycon of ident * typ list
    | TVariant of (ident * typ option) list
    | TRecord of (ident * typ * mutflag) list
and mutflag = Mutable | Immutable

type value =
    | VUnit | VNull | VBool of bool | VInt of int | VChar of char | VFloat of float
    | VString of string | VTuple of value list | VCons of value * value
    | VClosure of expr * expr * (value ref) Env.t
    | VBuiltin of (value -> value)

type symtab = {
    mutable env : value ref Env.t;
}

let token_to_string = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>" | ID id -> id | C_ID id -> id
    | BOOL_LIT b -> string_of_bool b | INT_LIT n -> string_of_int n
    | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'"
    | FLOAT_LIT f -> string_of_float f | STRING_LIT s -> "\"" ^ s ^ "\""
    | TVAR n -> "'" ^ string_of_int n
    | MODULE -> "module" | IMPORT -> "import" | AS -> "as" | DECL -> "decl"
    | TYPE -> "type" | AND -> "and" | LET -> "let" | REC -> "rec" | FUN -> "fun"
    | FN -> "fn" | IF -> "if" | THEN -> "then" | ELSE -> "else" | MATCH -> "match"
    | WHEN -> "when" | MUTABLE -> "mutable" | WILDCARD -> "_" | PLUS -> "+"
    | STAR -> "*" | PERCENT -> "%" | COMMA -> "," | SEMI -> ";" | DOT -> "."
    | RANGE -> ".." | MINUS -> "-" | RARROW -> "->" | EQ -> "=" | EQL -> "=="
    | NOT -> "!" | NEQ -> "!=" | LT -> "<" | LE -> "<=" | LARROW -> "<-"
    | GT -> ">" | GE -> ">=" | COLON -> ":" | ASSIGN -> ":=" | OR -> "|"
    | LOR -> "||" | AMP -> "&" | LAND -> "&&" | LSBRA -> "[" | RSBRA -> "]"
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
    | (_, Match (e, lst)) ->  "(match " ^ expr_to_string e ^ " {" ^ match_list_to_string lst ^ "})"
(*
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
and match_list_to_string = function
    | [] -> ""
    | (pat, e) :: rest ->
        " | " ^ pattern_to_string pat ^ " -> " ^ expr_to_string e ^ match_list_to_string rest
and pattern_to_string = function
    | PatNull -> "[]"
    | PatWildCard -> "_"
    | PatBool b -> string_of_bool b
    | PatInt n -> string_of_int n
    | PatChar c -> "'" ^ String.make 1 c ^ "'"
    | PatFloat f -> string_of_float f
    | PatString s -> "\"" ^ s ^ "\""
    | PatIdent id -> id
    | PatTuple pl -> "(" ^ pat_list_to_string pl ^ ")"
    | PatCons (p1, p2) -> pattern_to_string p1 ^ ":" ^ pattern_to_string p2
    | PatAs (pat, id) -> "(" ^ pattern_to_string pat ^ ") as " ^ id
    | PatOr (p1, p2) -> pattern_to_string p1 ^ " | " ^ pattern_to_string p2
and pat_list_to_string = function
    | [] -> ""
    | x::[] -> pattern_to_string x
    | x::xs -> pattern_to_string x ^ ", " ^ pat_list_to_string xs

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

