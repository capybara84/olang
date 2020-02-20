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
    print_newline ();
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
/* test
    /* nest */
*/

/*  6 */ identifier Ident 12345
/*  7 */ 'a' '\\t' \"abc\\n\" 'b
/*  8 */ module import as type

// comment

/* 12 */ let rec fun fn if then
/* 13 */ else match when mutable
/* 14 */ _ + * % , ; . ..
/* 15 */ - -> = == ! != < <= <- > >=
/* 16 */ : := | || & && [ ] []
/* 17 */ ( ) () { } /
/* 18 */"

let test_tokens = [
    (1,2,NEWLINE); (1,6,NEWLINE);
    (10,6, ID "identifier"); (21,6, C_ID "Ident"); (27,6, INT_LIT 12345); (1,7, NEWLINE);
    (10,7, CHAR_LIT 'a'); (14,7, CHAR_LIT '\t'); (19,7, STRING_LIT "abc\n"); (27,7, TVAR 1); (1,8, NEWLINE);
    (10,8, MODULE); (17,8, IMPORT); (24,8, AS); (27,8, TYPE); (1,10, NEWLINE); (1,12, NEWLINE);
    (10,12, LET); (14,12, REC); (18,12, FUN); (22,12, FN); (25,12, IF); (28,12, THEN); (1,13, NEWLINE);
    (10,13, ELSE); (15,13, MATCH); (21,13, WHEN); (26,13, MUTABLE); (1,14, NEWLINE);
    (10,14, WILDCARD); (12,14, PLUS); (14,14, STAR); (16,14, PERCENT); (18,14, COMMA);
    (20,14, SEMI); (22,14, DOT); (24,14, RANGE); (1,15, NEWLINE);
    (10,15, MINUS); (12,15, RARROW); (15,15, EQ); (17,15, EQL); (20,15, NOT); (22,15, NEQ);
    (25,15, LT); (27,15, LE); (30,15, LARROW); (33,15, GT); (35,15, GE); (1,16, NEWLINE);
    (10,16, COLON); (12,16, ASSIGN); (15,16, OR); (17,16, LOR); (20,16, AND);
    (22,16, LAND); (25,16, LSBRA); (27,16, RSBRA); (29,16, NULL); (1,17, NEWLINE);
    (10,17, LPAR); (12,17, RPAR); (14,17, UNIT); (17,17, LBRACE); (19,17, RBRACE);
    (21,17, SLASH); (1,18, NEWLINE); (9,18, EOF);
]

let scanner_test verbose =
    print_string "Scanner Test:";
    (try
        let tokens = Scanner.get_tokens @@ Scanner.from_string test_text in
        let len_tt = List.length test_tokens in
        let len_t = List.length tokens in
        test_eq len_tt len_t ("length " ^ string_of_int len_tt ^ " != " ^ string_of_int len_t);
        List.iter2 (fun (c,n,tt) t ->
            if verbose then begin
                Printf.printf "required [%d:%d '%s']\n" c n (token_to_string tt);
                Printf.printf "parsed [%d:%d '%s']\n" t.col t.line (token_to_string t.token)
            end;
            test_eq (c, n, tt) (t.col, t.line, t.token)
                (Printf.sprintf "(%d:%d,'%s') != (%d:%d,'%s')" c n (token_to_string tt)
                                    t.col t.line (token_to_string t.token)))
            test_tokens tokens
    with Invalid_argument s -> test_fail @@ "Invalid_argument " ^ s);
    print_newline ()


let test () =
    scanner_test !g_verbose;
    test_report ()

