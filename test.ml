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

let scanner_test_text = "
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

let scanner_test_tokens = [
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
        let tokens = Scanner.get_tokens @@ Scanner.from_string scanner_test_text in
        let len_tt = List.length scanner_test_tokens in
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
            scanner_test_tokens tokens
    with Invalid_argument s -> test_fail @@ "Invalid_argument " ^ s);
    print_newline ()


let parser_all_tests = [
    ("'a'", CharLit 'a');
    ("\"abc\"", StringLit "abc");
    ("12", IntLit 12);
    ("300 + 12",
        Binary (BinAdd, IntLit 300, IntLit 12));
    ("300 * 12 + 3",
        Binary (BinAdd, Binary (BinMul, IntLit 300, IntLit 12), IntLit 3));
    ("300 * (12 + 3)",
        Binary (BinMul, IntLit 300,
                Binary (BinAdd, IntLit 12, IntLit 3)));
    ("1 / 2 < 3 * 4",
        Binary (BinLT, (Binary (BinDiv, IntLit 1, IntLit 2)),
                        (Binary (BinMul, IntLit 3, IntLit 4))));
    ("2 * -(1 + 2)",
        Binary (BinMul, IntLit 2,
            (Unary (UMinus, Binary (BinAdd, IntLit 1, IntLit 2)))));
    ("5 % 2",
        Binary (BinMod, IntLit 5, IntLit 2));
    ("a && b",
        Binary (BinLand, Ident "a", Ident "b"));
    ("a || b",
        Binary (BinLor, Ident "a", Ident "b"));
    ("!(x < y)",
        Unary (UNot, Binary (BinLT, Ident "x", Ident "y")));
    ("1 <= 2",
        Binary (BinLE, IntLit 1, IntLit 2));
    ("1 > 2",
        Binary (BinGT, IntLit 1, IntLit 2));
    ("1 >= 2",
        Binary (BinGE, IntLit 1, IntLit 2));
    ("1 == 2",
        Binary (BinEql, IntLit 1, IntLit 2));
    ("1 != 2",
        Binary (BinNeq, IntLit 1, IntLit 2));
    ("fn x -> x + 1",
        Fn (Ident "x", Binary (BinAdd, Ident "x", IntLit 1)));
    ("f 3",
        Apply (Ident "f", IntLit 3));
    ("-(f 3)",
        Unary (UMinus, Apply (Ident "f", IntLit 3)));
    ("f (-3)",
        Apply (Ident "f", Unary (UMinus, IntLit 3)));
    ("f -3",
        Binary (BinSub, Ident "f", IntLit 3));
    ("fn () -> 1",
        Fn (Unit, IntLit 1));
    ("(fn x -> x + 1) (300 * (12 + 3))",
        Apply (Fn (Ident "x", Binary (BinAdd, Ident "x", IntLit 1)), 
            Binary (BinMul, IntLit 300,
                Binary (BinAdd, IntLit 12, IntLit 3))));
    ("let fact = fn n -> if n < 1 then 1 else n * fact (n - 1)",
            Let ("fact",
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", IntLit 1),
                          IntLit 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                IntLit 1))))))));
    ("{}",
        Comp []);
    ("{1; 2; }",
        Comp [IntLit 1; IntLit 2]);
    ("{1; 2; 3}",
        Comp [IntLit 1; IntLit 2; IntLit 3]);
    ("let fact = fn n -> if n < 1 then 1 else n * fact (n - 1)",
        Let ("fact",
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", IntLit 1),
                          IntLit 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                IntLit 1))))))));
    ("1+2+3",
        (Binary (BinAdd,
            (Binary (BinAdd, IntLit 1, IntLit 2)), IntLit 3)));
    ("f 1 2",
        Apply (Apply (Ident "f", IntLit 1), IntLit 2));
    ("f 1 2 3",
        (Apply (Apply (Apply (Ident "f",
            IntLit 1), IntLit 2), IntLit 3)));
    ("1:2:3:[]",
        (Binary (BinCons, IntLit 1,
                 (Binary (BinCons, IntLit 2,
                          (Binary (BinCons, IntLit 3, Null)))))));
    ("[1,2,3]",
        (Binary (BinCons, IntLit 1,
                 (Binary (BinCons, IntLit 2,
                          (Binary (BinCons, IntLit 3, Null)))))));
    ("(1)", IntLit 1);
    ("true", BoolLit true);
    ("false", BoolLit false);
    ("fun one () = 1",
        LetRec ("one", (Fn (Unit, IntLit 1))));
    ("fun fact n = if n < 1 then 1 else n * fact (n-1)",
        LetRec ("fact",
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", IntLit 1),
                          IntLit 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                IntLit 1))))))));
    ("module List", Module "List");
    ("import Array", Import ("Array", None));
    ("import Array as A", Import ("Array", Some "A"));
    ("Array.length", IdentMod ("Array", (Ident "length")));
    ("(1)", IntLit 1);
    ("(1,2)", Tuple [IntLit 1; IntLit 2]);
    ("(1,2,3)", Tuple [IntLit 1; IntLit 2; IntLit 3]);
]

let parser_test verbose =
    print_string "Parser Test:";
    let do_parse (text, expected) =
        try
            if verbose then 
                print_endline ("text    > " ^ text)
            else ();
            let expr = Parser.parse @@ Scanner.from_string text in
            let parsed = expr_to_string expr in
            let expected = expr_to_string expected in
            if verbose then begin
                print_endline ("parsed  > " ^ parsed);
                print_endline ("expected> " ^ expected)
            end else ();
            test_eq parsed expected ("text:" ^ text ^ "\nresult:" ^ parsed ^ " != " ^ expected)
        with Error s -> test_fail s
    in
    List.iter do_parse parser_all_tests;
    print_newline ()

let test () =
    scanner_test !g_verbose;
    parser_test !g_verbose;
    test_report ()

