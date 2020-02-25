
exception Error of string

let g_verbose = ref false

type ident = string

type token
    = EOF | NEWLINE | ID of ident | C_ID of ident | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | FLOAT_LIT of float | STRING_LIT of string | TVAR of int
    | MODULE | IMPORT | AS | DECL | TYPE | AND | LET | REC | FUN | FN | IF | THEN
    | ELSE | MATCH | WHEN | MUTABLE | WILDCARD | PLUS | STAR | PERCENT | COMMA
    | SEMI | DOT | RANGE | MINUS | RARROW | EQ | EQL | NOT | NEQ | LT | LE
    | LARROW | GT | GE | COLON | DCOLON | ASSIGN | OR | LOR | AMP | LAND | LSBRA | RSBRA
    | NULL | LPAR | RPAR | UNIT | LBRACE | RBRACE | SLASH

type token_t = {
    token : token;
    line : int;
    col : int;
}

type type_name = ident list * ident

type typ =
    | TUnit | TBool | TInt | TChar | TFloat | TString
    | TConstr of type_name * typ option
    | TTuple of typ list
    | TFun of typ * typ
    | TVar of int * typ option ref
and type_decl =
    | TTypeDecl of typ
    | TVariantDecl of (ident * typ option) list
    | TRecordDecl of (ident * typ * mutflag) list
and mutflag = Mutable | Immutable

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
    | TypeDecl of ident * int list * type_decl
    | TypeDeclAnd of expr * expr
    | Module of ident
    | Import of ident * ident option
and
    expr = int * exp


type value =
    | VUnit | VNull | VBool of bool | VInt of int | VChar of char | VFloat of float
    | VString of string | VTuple of value list | VCons of value * value
    | VClosure of expr * expr * (value ref) Env.t
    | VBuiltin of (value -> value)

type symtab = {
    mutable env : value ref Env.t;
}

let tvar_to_string n =
    "'" ^
        if n <= Char.code 'z' - Char.code 'a' then
            String.make 1 (Char.chr ((Char.code 'a') + n))
        else
            string_of_int n

let token_to_string = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>" | ID id -> id | C_ID id -> id
    | BOOL_LIT b -> string_of_bool b | INT_LIT n -> string_of_int n
    | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'"
    | FLOAT_LIT f -> string_of_float f | STRING_LIT s -> "\"" ^ s ^ "\""
    | TVAR n -> tvar_to_string n
    | MODULE -> "module" | IMPORT -> "import" | AS -> "as" | DECL -> "decl"
    | TYPE -> "type" | AND -> "and" | LET -> "let" | REC -> "rec" | FUN -> "fun"
    | FN -> "fn" | IF -> "if" | THEN -> "then" | ELSE -> "else" | MATCH -> "match"
    | WHEN -> "when" | MUTABLE -> "mutable" | WILDCARD -> "_" | PLUS -> "+"
    | STAR -> "*" | PERCENT -> "%" | COMMA -> "," | SEMI -> ";" | DOT -> "."
    | RANGE -> ".." | MINUS -> "-" | RARROW -> "->" | EQ -> "=" | EQL -> "=="
    | NOT -> "!" | NEQ -> "!=" | LT -> "<" | LE -> "<=" | LARROW -> "<-"
    | GT -> ">" | GE -> ">=" | COLON -> ":" | DCOLON -> "::" | ASSIGN -> ":="
    | OR -> "|" | LOR -> "||" | AMP -> "&" | LAND -> "&&" | LSBRA -> "[" | RSBRA -> "]"
    | NULL -> "[]" | LPAR -> "(" | RPAR -> ")" | UNIT -> "()" | LBRACE -> "{"
    | RBRACE -> "}" | SLASH -> "/"

let rec tname_to_string (idl, id) =
    match idl with
    | [] -> id
    | x::xs -> x ^ "." ^ tname_to_string (xs, id)

let rec type_to_string = function
    | TUnit -> "unit" | TBool -> "bool" | TInt -> "int" | TChar -> "char"
    | TFloat -> "float" | TString -> "string"
    | TConstr (name, Some t) -> "(" ^ tname_to_string name ^ " " ^ type_to_string t ^ ")"
    | TConstr (name, None) -> "(" ^ tname_to_string name ^ ")"
    | TTuple tl -> "(" ^ tuple_to_string tl ^ ")"
    | TFun (t1, t2) -> "(" ^ type_to_string t1 ^ " -> " ^ type_to_string t2 ^ ")"
    | TVar (n, {contents = None}) -> tvar_to_string n
    | TVar (_, {contents = Some t}) -> type_to_string t
and tuple_to_string = function
    | [] -> ""
    | t::[] -> type_to_string t
    | t::ts -> type_to_string t ^ " * " ^ tuple_to_string ts

let rec type_decl_to_string = function
    | TTypeDecl t -> type_to_string t
    | TVariantDecl vl -> variant_to_string vl
    | TRecordDecl fl -> record_to_string fl
and variant_to_string = function
    | [] -> ""
    | (id, None)::xs ->
        " | " ^ id ^ variant_to_string xs
    | (id, Some t)::xs ->
        " | " ^ id ^ " " ^ type_to_string t ^ variant_to_string xs
and record_to_string = function
    | [] -> ""
    | (id, t, mut)::xs ->
        (if mut = Mutable then " mutable " else " ") ^ id ^ " :: " ^ type_to_string t ^ ";"
            ^ record_to_string xs


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
    | (_, TypeDecl (id, tvl, td)) ->
        "(type " ^ id ^ tvlist_to_string tvl ^ " = " ^ type_decl_to_string td ^ ")"
    | (_, TypeDeclAnd (e1, e2)) -> expr_to_string e1 ^ " and " ^ expr_to_string e2
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
and tvlist_to_string = function
    | [] -> ""
    | x::[] -> " " ^ tvar_to_string x
    | tvl -> " (" ^ tvar_list_to_string tvl  ^ ")"
and tvar_list_to_string = function
    | [] -> ""
    | x::[] -> tvar_to_string x
    | x::xs -> tvar_to_string x ^ ", " ^ tvar_list_to_string xs


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
        ( match rhs with
        | VNull -> value_to_string lhs
        | VCons _ -> value_to_string lhs ^ ", " ^ vcons_to_string rhs
        | _ -> failwith "cons rhs bug")
    | _ -> failwith "cons bug"

let rec expr_to_string_src = function
    | (_, Eof) -> "Eof" | (_, Unit) -> "Unit" | (_, Null) -> "Null" | (_, WildCard) -> "WildCard"
    | (_, BoolLit b) -> "(BoolLit " ^ string_of_bool b ^ ")"
    | (_, IntLit n) -> "(IntLit " ^ string_of_int n ^ ")"
    | (_, CharLit c) -> "(CharLit '" ^ String.make 1 c ^ "')"
    | (_, FloatLit f) -> "(FloatLit " ^ string_of_float f ^ ")"
    | (_, StringLit s) -> "(StringLit \"" ^ s ^ "\")"
    | (_, Ident id) -> "(Ident \"" ^ id ^ "\")"
    | (_, IdentMod (id, e)) -> "(IdentMod (\"" ^ id ^ "\", " ^ expr_to_string_src e ^ ")"
    | (_, Record (e, id)) -> "(Record (" ^ expr_to_string_src e ^ ", \"" ^ id ^ "\"))"
    | (_, Tuple el) -> "(Tuple (" ^ tuple_to_string_src el ^ "))"
    | (_, Binary (op, lhs, rhs)) -> "(Binary (" ^ string_of_binop op ^ ", "
        ^ expr_to_string_src lhs ^ ", " ^ expr_to_string_src rhs ^ "))"
    | (_, Unary (op, e)) -> "(Unary (" ^ string_of_unop_src op ^ ", " ^ expr_to_string_src e ^ "))"
    | (_, Let (id, e)) -> "(Let (\"" ^ id ^ "\", " ^ expr_to_string_src e ^ "))"
    | (_, LetRec (id, e)) -> "(LetRec (\"" ^ id ^ "\", " ^ expr_to_string_src e ^ "))"
    | (_, Fn (e1, e2)) -> "(Fn (" ^ expr_to_string_src e1 ^ ", " ^ expr_to_string_src e2 ^ "))"
    | (_, Apply (e1, e2)) -> "(Apply (" ^ expr_to_string_src e1 ^ ", " ^ expr_to_string_src e2 ^ "))"
    | (_, If (e1, e2, e3)) -> "(If (" ^ expr_to_string_src e1 ^ ", " ^ expr_to_string_src e2
        ^ ", " ^ expr_to_string_src e3 ^ "))"
    | (_, Comp el) -> "(Comp " ^ comp_to_string_src el ^ ")"
    | (_, Match (e, lst)) ->  "(Match (" ^ expr_to_string_src e ^ ", "
        ^ match_list_to_string_src lst ^ "))"
    | (_, TypeDecl (id, tvl, td)) ->
        "(TypeDecl (\"" ^ id ^ "\", " ^ tvlist_to_string_src tvl ^ ", "
            ^ type_decl_to_string_src td ^ "))"
    | (_, TypeDeclAnd (e1, e2)) -> "(" ^ expr_to_string_src e1 ^ ", " ^ expr_to_string e2 ^ ")"
    | (_, Module name) -> "(Module " ^ name ^ ")"
    | (_, Import (name, Some rename)) -> "(Import (" ^ name ^ ", Some " ^ rename ^ "))"
    | (_, Import (name, None)) -> "(Import " ^ name ^ ", None)"
and tuple_to_string e in
and match_list_to_string_src lst in
and tvlist_to_string_src lhs in
and type_decl_to_string td
