
exception Error of string

let g_verbose = ref false

type token
    = EOF | NEWLINE | ID of string | C_ID of string | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | FLOAT_LIT of float | STRING_LIT of string | TVAR of int
    | MODULE | IMPORT | AS | TYPE | LET | REC | FUN | FN | IF | THEN
    | ELSE | MATCH | WHEN | MUTABLE
    | WILDCARD | PLUS | STAR | PERCENT | DOT | COMMA | SEMI
    | MINUS | RARROW | EQ | EQL | NOT | NEQ | LT | LE | LARROW | GT | GE
    | COLON | ASSIGN | OR | LOR | AND | LAND | LSBRA | RSBRA | NULL
    | LPAR | RPAR | UNIT | LBRACE | RBRACE | SLASH

type token_t = {
    token : token;
    line : int;
    col : int;
}

let token_to_string = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>" | ID id -> id | C_ID id -> id
    | BOOL_LIT b -> string_of_bool b | INT_LIT n -> string_of_int n
    | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'"
    | FLOAT_LIT f -> string_of_float f | STRING_LIT s -> "\"" ^ s ^ "\""
    | TVAR n -> "'" ^ string_of_int n
    | MODULE -> "module" | IMPORT -> "import" | AS -> "as" | TYPE -> "type" 
    | LET -> "let" | REC -> "rec" | FUN -> "fun" | FN -> "fn" | IF -> "if"
    | THEN -> "then" | ELSE -> "else" | MATCH -> "match" | WHEN -> "when" | MUTABLE -> "mutable"
    | WILDCARD -> "_" | PLUS -> "+" | STAR -> "*" | PERCENT -> "%" | DOT -> "." | COMMA -> ","
    | SEMI -> ";" | MINUS -> "-" | RARROW -> "->" | EQ -> "=" | EQL -> "=="
    | NOT -> "!" | NEQ -> "!=" | LT -> "<" | LE -> "<=" | LARROW -> "<-"
    | GT -> ">" | GE -> ">=" | COLON -> ":" | ASSIGN -> ":=" | OR -> "|"
    | LOR -> "||" | AND -> "&" | LAND -> "&&" | LSBRA -> "[" | RSBRA -> "]"
    | NULL -> "[]" | LPAR -> "(" | RPAR -> ")" | UNIT -> "()" | LBRACE -> "{"
    | RBRACE -> "}" | SLASH -> "/"

