open Syntax

let color c s = "\x1b[3" ^ (string_of_int c) ^ "m" ^ s ^ "\x1b[0m"
let red = color 1
let green = color 2
let yellow = color 3
let blue = color 4
let magenta = color 5
let cyan = color 6
let white = color 7

let n_ok = ref 0
let n_fail = ref 0

let test_ok () =
    incr n_ok;
    print_string @@ green "."

let test_fail s =
    incr n_fail;
    print_endline @@ red "!" ^ s

let test_eq a b m =
    if a = b then
        test_ok ()
    else
        test_fail m

let test_report () =
    let n_all = !n_ok + !n_fail in
    print_endline ("All   : " ^ (string_of_int n_all));
    print_endline ("OK    : " ^ (green @@ string_of_int !n_ok));
    print_endline ("Failed: " ^ (if !n_fail = 0 then "0" else (red @@ string_of_int !n_fail)))


(*------------------------*)

let test_text = "
identifier Ident 12345
'a' '\\t' \"abc\\n\" 'b
module import as type
let rec fun fn if then
else match when mutable
_ + * % . , ;
- -> = == ! != < <= <- > >=
: := | || & && [ ] []
( ) () { } /
"

let test_tokens = [
    NEWLINE; ID "identifier"; C_ID "Ident"; INT_LIT 12345; NEWLINE;
    CHAR_LIT 'a'; CHAR_LIT '\t'; STRING_LIT "abc\n"; TVAR 1; NEWLINE;
    MODULE; IMPORT; AS; TYPE; NEWLINE;
    LET; REC; FUN; FN; IF; THEN; NEWLINE;
    ELSE; MATCH; WHEN; MUTABLE; NEWLINE;
    WILDCARD; PLUS; STAR; PERCENT; DOT; COMMA; SEMI; NEWLINE;
    MINUS; RARROW; EQ; EQL; NOT; NEQ; LT; LE; LARROW; GT; GE; NEWLINE;
    COLON; ASSIGN; OR; LOR; AND; LAND; LSBRA; RSBRA; NULL; NEWLINE;
    LPAR; RPAR; UNIT; LBRACE; RBRACE; SLASH; NEWLINE;
    EOF
]

let scanner_test verbose =
    print_string "Scanner Test:";
    let tokens = Scanner.get_tokens @@ Scanner.from_string test_text in
    let len_tt = List.length test_tokens in
    let len_t = List.length tokens in
    test_eq len_tt len_t ("length " ^ string_of_int len_tt ^ " != " ^ string_of_int len_t);
    if verbose then
        List.iter (fun x ->
                print_endline ("[" ^ token_to_string x.token ^ ", "
                    ^ string_of_int x.col ^ ":" ^ string_of_int x.line ^ "]")) tokens;
    List.iter2 (fun tt t -> test_eq tt t.token
                ((token_to_string tt) ^ " != " ^ (token_to_string t.token)))
            test_tokens tokens;
    print_newline ()


let test () =
    scanner_test !g_verbose;
    test_report ()
