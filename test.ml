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
        let tokens = Scanner.get_tokens @@ Scanner.from_string "TEST" scanner_test_text in
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
    with Invalid_argument s -> test_fail @@ "Invalid_argument " ^ s
        | Error s -> test_fail s);
    print_newline ()


let parser_all_tests = [
    ("'a'", (1, CharLit 'a'));
    ("\"abc\"", (1, StringLit "abc"));
    ("12", (1, IntLit 12));
    ("300 + 12",
        (1, Binary (BinAdd, (1, IntLit 300), (1, IntLit 12))));
    ("300 * 12 + 3",
        (1, Binary (BinAdd,
            (1, Binary (BinMul, (1, IntLit 300), (1, IntLit 12))), (1, IntLit 3))));
    ("300 * (12 + 3)",
        (1, Binary (BinMul, (1, IntLit 300),
                (1, Binary (BinAdd, (1, IntLit 12), (1, IntLit 3))))));
    ("1 / 2 < 3 * 4",
        (1, Binary (BinLT, (1, (Binary (BinDiv, (1, IntLit 1), (1, IntLit 2)))),
                        (1, (Binary (BinMul, (1, IntLit 3), (1, IntLit 4)))))));
    ("2 * -(1 + 2)",
        (1, Binary (BinMul, (1, IntLit 2),
            (1, (Unary (UMinus, (1, Binary (BinAdd, (1, IntLit 1), (1, IntLit 2)))))))));
    ("5 % 2",
        (1, Binary (BinMod, (1, IntLit 5), (1, IntLit 2))));
    ("a && b",
        (1, Binary (BinLand, (1, Ident "a"), (1, Ident "b"))));
    ("a || b",
        (1, Binary (BinLor, (1, Ident "a"), (1, Ident "b"))));
    ("!(x < y)",
        (1, Unary (UNot, (1, Binary (BinLT, (1, Ident "x"), (1, Ident "y"))))));
    ("1 <= 2",
        (1, Binary (BinLE, (1, IntLit 1), (1, IntLit 2))));
    ("1 > 2",
        (1, Binary (BinGT, (1, IntLit 1), (1, IntLit 2))));
    ("1 >= 2",
        (1, Binary (BinGE, (1, IntLit 1), (1, IntLit 2))));
    ("1 == 2",
        (1, Binary (BinEql, (1, IntLit 1), (1, IntLit 2))));
    ("1 != 2",
        (1, Binary (BinNeq, (1, IntLit 1), (1, IntLit 2))));
    ("fn x -> x + 1",
        (1, Fn ((1, Ident "x"), (1, Binary (BinAdd, (1, Ident "x"), (1, IntLit 1))))));
    ("f 3",
        (1, Apply ((1, Ident "f"), (1, IntLit 3))));
    ("-(f 3)",
        (1, Unary (UMinus, (1, Apply ((1, Ident "f"), (1, IntLit 3))))));
    ("f (-3)",
        (1, Apply ((1, Ident "f"), (1, Unary (UMinus, (1, IntLit 3))))));
    ("f -3",
        (1, Binary (BinSub, (1, Ident "f"), (1, IntLit 3))));
    ("fn () -> 1",
        (1, Fn ((1, Unit), (1, IntLit 1))));
    ("(fn x -> x + 1) (300 * (12 + 3))",
        (1, Apply ((1, Fn ((1, Ident "x"),
                            (1, Binary (BinAdd, (1, Ident "x"), (1, IntLit 1))))), 
            (1, Binary (BinMul, (1, IntLit 300),
                (1, Binary (BinAdd, (1, IntLit 12), (1, IntLit 3))))))));
    ("let rec fact = fn n -> if n < 1 then 1 else n * fact (n - 1)",
        (1, LetRec ("fact",
            (1, Fn ((1, Ident "n"),
                    (1, If ((1, Binary (BinLT, (1, Ident "n"), (1, IntLit 1))),
                          (1, IntLit 1),
                          (1, Binary (BinMul, (1, Ident "n"),
                                        (1, Apply ((1, Ident "fact"),
                                            (1, Binary (BinSub, (1, Ident "n"),
                                                (1, IntLit 1))))))))))))));
    ("{}",
        (1, Comp []));
    ("{1; 2; }",
        (1, Comp [(1, IntLit 1); (1, IntLit 2)]));
    ("{1; 2; 3}",
        (1, Comp [(1, IntLit 1); (1, IntLit 2); (1, IntLit 3)]));
    ("f 1 2",
        (1, Apply ((1, Apply ((1, Ident "f"), (1, IntLit 1))), (1, IntLit 2))));
    ("1:2:3:[]",
        (1, (Binary (BinCons, (1, IntLit 1),
                 (1, (Binary (BinCons, (1, IntLit 2),
                          (1, (Binary (BinCons, (1, IntLit 3), (1, Null)))))))))));
    ("[1,2,3]",
        (1, (Binary (BinCons, (1, IntLit 1),
                 (1, (Binary (BinCons, (1, IntLit 2),
                          (1, (Binary (BinCons, (1, IntLit 3), (1, Null)))))))))));
    ("(1)", (1, IntLit 1));
    ("true", (1, BoolLit true));
    ("false", (1, BoolLit false));
    ("fun one () = 1",
        (1, LetRec ("one", (1, (Fn ((1, Unit), (1, IntLit 1)))))));
    ("fun fact n = if n < 1 then 1 else n * fact (n-1)",
        (1, LetRec ("fact",
            (1, Fn ((1, Ident "n"),
                    (1, If ((1, Binary (BinLT, (1, Ident "n"), (1, IntLit 1))),
                          (1, IntLit 1),
                          (1, Binary (BinMul, (1, Ident "n"),
                                        (1, Apply ((1, Ident "fact"),
                                            (1, Binary (BinSub, (1, Ident "n"),
                                                (1, IntLit 1))))))))))))));
    ("module List", (1, Module "List"));
    ("import Array", (1, Import ("Array", None)));
    ("import Array as A", (1, Import ("Array", Some "A")));
    ("Array.length", (1, IdentMod ("Array", (1, Ident "length"))));
    ("(1)", (1, IntLit 1));
    ("(1,2)", (1, Tuple [(1, IntLit 1); (1, IntLit 2)]));
    ("(1,2,3)", (1, Tuple [(1, IntLit 1); (1, IntLit 2); (1, IntLit 3)]));
]

let parser_test verbose =
    print_string "Parser Test:";
    let do_parse (text, expected) =
        try
            if verbose then 
                print_endline ("text    > " ^ text)
            else ();
            let expr = Parser.parse @@ Scanner.from_string "TEST" text in
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

let eval_all_tests = [
    ("12", VInt 12);
    ("'a'", VChar 'a');
    ("\"abc\"", VString "abc");
    ("300 + 12", VInt 312);
    ("300 * 12 + 3", VInt 3603);
    ("300 * (12 + 3)", VInt 4500);
    ("300 / (12 - 3)", VInt 33);
    ("300 % (12 - 3)", VInt 3);
    ("1 < 2", VBool true);
    ("1 <= 1", VBool true);
    ("1 > 2", VBool false);
    ("2 >= 2", VBool true);
    ("2 == 2", VBool true);
    ("2 == 1", VBool false);
    ("2 != 1", VBool true);
    ("2 != 2", VBool false);
    ("'a' == 'a'", VBool true);
    ("'a' == 'b'", VBool false);
    ("'a' != 'a'", VBool false);
    ("'a' != 'b'", VBool true);
    ("'a' < 'b'", VBool true);
    ("'b' < 'a'", VBool false);
    ("'a' <= 'a'", VBool true);
    ("'b' <= 'a'", VBool false);
    ("'a' > 'b'", VBool false);
    ("'b' > 'a'", VBool true);
    ("'a' >= 'b'", VBool false);
    ("'a' >= 'a'", VBool true);
    ("\"abc\" + \"def\"", VString "abcdef");
    ("\"abc\" == \"abc\"", VBool true);
    ("\"abc\" == \"def\"", VBool false);
    ("\"abc\" != \"abc\"", VBool false);
    ("\"abc\" != \"def\"", VBool true);
    ("\"abc\" < \"def\"", VBool true);
    ("\"abc\" > \"def\"", VBool false);
    ("1 > 2 || 2 > 1", VBool true);
    ("1 < 2 && 2 < 1", VBool false);
    ("-5", VInt (-5));
    ("!true", VBool false);
    ("!false", VBool true);
    ("(1,2)", VTuple [VInt 1; VInt 2]);
    ("(1,'a')", VTuple [VInt 1; VChar 'a']);
    ("(1,'a',\"abc\")", VTuple [VInt 1; VChar 'a'; VString "abc"]);
    ("1:[2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("1:2:[3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3] == 1:[2,3]", VBool true);
    ("[1,2,3] == 1:[2,3,4]", VBool false);
    ("let x = 1", VUnit);
    ("x", VInt 1);
    ("let f = fn () -> 5", VUnit);
    ("f ()", VInt 5);
    ("let g = fn _ -> 8", VUnit);
    ("g 3", VInt 8);
    ("let a = fn x -> x + 1", VUnit);
    ("a 4", VInt 5);
    ("let add = fn x -> fn y -> x + y", VUnit);
    ("add 1 2", VInt 3);
    ("let add5 = add 5", VUnit);
    ("add5 3", VInt 8);
    ("fun foo x = x + 2", VUnit);
    ("foo 4", VInt 6);
    ("fun fact n = if n < 1 then 1 else n * fact (n-1)", VUnit);
    ("fact 5", VInt 120);
(*
    ("module A", VUnit);
    ("let x = 1", VUnit);
    ("module B", VUnit);
    ("let x = 2", VUnit);
    ("module Main", VUnit);
    ("A.x", VInt 1);
    ("B.x", VInt 2);
    ("import List", VUnit);
    ("List.length [1,2,3]", VInt 3);
    ("import List as L", VUnit);
    ("L.length [1,2,3,4]", VInt 4);
*)
(*
    ("fst (1,2)", VInt 1);
    ("snd (1,2)", VInt 2);
*)
(*
    ("(fn n -> match n { 0 -> 'a' | 1 -> 'b' | 2 -> 'c' }) 1", VChar 'b');
    ("(fn n -> match n { (_,_,x) -> x }) (1,2,3)", VInt 3);
    ("(fn n -> match n { 0 | 1 | 2 -> 'a' | 3 -> 'b' }) 1", VChar 'a');
    ("(fn n -> match n { 0 | 1 | 2 -> 'a' | 3 -> 'b' }) 2", VChar 'a');
    ("(fn n -> match n { 0 | 1 | 2 -> 'a' | 3 -> 'b' }) 3", VChar 'b');
    ("match [1,2,3] { [a,b,c] -> a }", VInt 1);
    ("match [1,2,3] { [a,b,c] as d -> a }", VInt 1);
    ("match [1,2,3] { [a,b,c] as d -> b }", VInt 2);
    ("match [1,2,3] { [a,b,c] as d -> c }", VInt 3);
    ("match [1,2,3] { [a,b,c] as d -> d }",
        VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("match [1,2,3] { x:xs -> x }", VInt 1);
    ("match [1,2,3] { x:y:xs -> y }", VInt 2);
    ("match [1,2,3] { x:xs -> xs }", VCons(VInt 2, VCons (VInt 3, VNull)));
*)
]

let eval_test verbose env =
    print_string "Eval Test:";
    let env = ref env in
    let do_eval (text, expected) =
        try
            if verbose then
                print_endline ("text> " ^ text)
            else ();
            let expr = Parser.parse @@ Scanner.from_string "TEST" text in
            let (new_env, v) = Eval.eval_decl !env expr in
            env := new_env;
            if verbose then begin
                print_endline ("evaluated> " ^ value_to_string v);
                print_endline ("expected > " ^ value_to_string expected)
            end;
            test_eq v expected
                ("text: " ^ text ^ ", result: " ^ value_to_string v ^ " != " ^ value_to_string expected)
        with Error s -> test_fail s
    in
    List.iter do_eval eval_all_tests;
    print_newline ()

let test env =
    scanner_test !g_verbose;
    parser_test !g_verbose;
    eval_test !g_verbose env;
    test_report ()

