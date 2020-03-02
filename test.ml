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
/*  8 */ module import as decl type and

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
    (10,8, MODULE); (17,8, IMPORT); (24,8, AS); (27,8, DECL); (32,8, TYPE); (37,8, AND);
    (1,10, NEWLINE); (1,12, NEWLINE);
    (10,12, LET); (14,12, REC); (18,12, FUN); (22,12, FN); (25,12, IF); (28,12, THEN); (1,13, NEWLINE);
    (10,13, ELSE); (15,13, MATCH); (21,13, WHEN); (26,13, MUTABLE); (1,14, NEWLINE);
    (10,14, WILDCARD); (12,14, PLUS); (14,14, STAR); (16,14, PERCENT); (18,14, COMMA);
    (20,14, SEMI); (22,14, DOT); (24,14, RANGE); (1,15, NEWLINE);
    (10,15, MINUS); (12,15, RARROW); (15,15, EQ); (17,15, EQL); (20,15, NOT); (22,15, NEQ);
    (25,15, LT); (27,15, LE); (30,15, LARROW); (33,15, GT); (35,15, GE); (1,16, NEWLINE);
    (10,16, COLON); (12,16, ASSIGN); (15,16, OR); (17,16, LOR); (20,16, AMP);
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
                Printf.printf "parsed [%d:%d '%s']\n" t.pos.col t.pos.line (token_to_string t.token)
            end;
            test_eq (c, n, tt) (t.pos.col, t.pos.line, t.token)
                (Printf.sprintf "(%d:%d,'%s') != (%d:%d,'%s')" c n (token_to_string tt)
                    t.pos.col t.pos.line (token_to_string t.token)))
            scanner_test_tokens tokens
    with Invalid_argument s -> test_fail @@ "Invalid_argument " ^ s
        | Error s -> test_fail s);
    print_newline ()


let dummy = {filename=""; line=1; col=1;}

let parser_all_tests = [
    ("'a'", (dummy, CharLit 'a'));
    ("\"abc\"", (dummy, StringLit "abc"));
    ("12", (dummy, IntLit 12));
    ("300 + 12",
        (dummy, Binary (BinAdd, (dummy, IntLit 300), (dummy, IntLit 12))));
    ("300 * 12 + 3",
        (dummy, Binary (BinAdd,
            (dummy, Binary (BinMul, (dummy, IntLit 300), (dummy, IntLit 12))), (dummy, IntLit 3))));
    ("300 * (12 + 3)",
        (dummy, Binary (BinMul, (dummy, IntLit 300),
                (dummy, Binary (BinAdd, (dummy, IntLit 12), (dummy, IntLit 3))))));
    ("1 / 2 < 3 * 4",
        (dummy, Binary (BinLT, (dummy, (Binary (BinDiv, (dummy, IntLit 1), (dummy, IntLit 2)))),
                        (dummy, (Binary (BinMul, (dummy, IntLit 3), (dummy, IntLit 4)))))));
    ("2 * -(1 + 2)",
        (dummy, Binary (BinMul, (dummy, IntLit 2),
            (dummy, (Unary (UMinus, (dummy, Binary (BinAdd, (dummy, IntLit 1), (dummy, IntLit 2)))))))));
    ("5 % 2",
        (dummy, Binary (BinMod, (dummy, IntLit 5), (dummy, IntLit 2))));
    ("a && b",
        (dummy, Binary (BinLand, (dummy, Ident "a"), (dummy, Ident "b"))));
    ("a || b",
        (dummy, Binary (BinLor, (dummy, Ident "a"), (dummy, Ident "b"))));
    ("!(x < y)",
        (dummy, Unary (UNot, (dummy, Binary (BinLT, (dummy, Ident "x"), (dummy, Ident "y"))))));
    ("1 <= 2",
        (dummy, Binary (BinLE, (dummy, IntLit 1), (dummy, IntLit 2))));
    ("1 > 2",
        (dummy, Binary (BinGT, (dummy, IntLit 1), (dummy, IntLit 2))));
    ("1 >= 2",
        (dummy, Binary (BinGE, (dummy, IntLit 1), (dummy, IntLit 2))));
    ("1 == 2",
        (dummy, Binary (BinEql, (dummy, IntLit 1), (dummy, IntLit 2))));
    ("1 != 2",
        (dummy, Binary (BinNeq, (dummy, IntLit 1), (dummy, IntLit 2))));
    ("fn x -> x + 1",
        (dummy, Fn ((dummy, Ident "x"), (dummy, Binary (BinAdd, (dummy, Ident "x"), (dummy, IntLit 1))))));
    ("f 3",
        (dummy, Apply ((dummy, Ident "f"), (dummy, IntLit 3))));
    ("-(f 3)",
        (dummy, Unary (UMinus, (dummy, Apply ((dummy, Ident "f"), (dummy, IntLit 3))))));
    ("f (-3)",
        (dummy, Apply ((dummy, Ident "f"), (dummy, Unary (UMinus, (dummy, IntLit 3))))));
    ("f -3",
        (dummy, Binary (BinSub, (dummy, Ident "f"), (dummy, IntLit 3))));
    ("fn () -> 1",
        (dummy, Fn ((dummy, Unit), (dummy, IntLit 1))));
    ("(fn x -> x + 1) (300 * (12 + 3))",
        (dummy, Apply ((dummy, Fn ((dummy, Ident "x"),
                            (dummy, Binary (BinAdd, (dummy, Ident "x"), (dummy, IntLit 1))))), 
            (dummy, Binary (BinMul, (dummy, IntLit 300),
                (dummy, Binary (BinAdd, (dummy, IntLit 12), (dummy, IntLit 3))))))));
    ("let rec fact = fn n -> if n < 1 then 1 else n * fact (n - 1)",
        (dummy, LetRec ("fact",
            (dummy, Fn ((dummy, Ident "n"),
                    (dummy, If ((dummy, Binary (BinLT, (dummy, Ident "n"), (dummy, IntLit 1))),
                          (dummy, IntLit 1),
                          (dummy, Binary (BinMul, (dummy, Ident "n"),
                                        (dummy, Apply ((dummy, Ident "fact"),
                                            (dummy, Binary (BinSub, (dummy, Ident "n"),
                                                (dummy, IntLit 1))))))))))))));
    ("{}",
        (dummy, Comp []));
    ("{1; 2; }",
        (dummy, Comp [(dummy, IntLit 1); (dummy, IntLit 2)]));
    ("{1; 2; 3}",
        (dummy, Comp [(dummy, IntLit 1); (dummy, IntLit 2); (dummy, IntLit 3)]));
    ("f 1 2",
        (dummy, Apply ((dummy, Apply ((dummy, Ident "f"), (dummy, IntLit 1))), (dummy, IntLit 2))));
    ("1:2:3:[]",
        (dummy, (Binary (BinCons, (dummy, IntLit 1),
                 (dummy, (Binary (BinCons, (dummy, IntLit 2),
                          (dummy, (Binary (BinCons, (dummy, IntLit 3), (dummy, Null)))))))))));
    ("[1,2,3]",
        (dummy, (Binary (BinCons, (dummy, IntLit 1),
                 (dummy, (Binary (BinCons, (dummy, IntLit 2),
                          (dummy, (Binary (BinCons, (dummy, IntLit 3), (dummy, Null)))))))))));
    ("(1)", (dummy, IntLit 1));
    ("true", (dummy, BoolLit true));
    ("false", (dummy, BoolLit false));
    ("fun one () = 1",
        (dummy, LetRec ("one", (dummy, (Fn ((dummy, Unit), (dummy, IntLit 1)))))));
    ("fun fact n = if n < 1 then 1 else n * fact (n-1)",
        (dummy, LetRec ("fact",
            (dummy, Fn ((dummy, Ident "n"),
                    (dummy, If ((dummy, Binary (BinLT, (dummy, Ident "n"), (dummy, IntLit 1))),
                          (dummy, IntLit 1),
                          (dummy, Binary (BinMul, (dummy, Ident "n"),
                                        (dummy, Apply ((dummy, Ident "fact"),
                                            (dummy, Binary (BinSub, (dummy, Ident "n"),
                                                (dummy, IntLit 1))))))))))))));
    ("module List", (dummy, Module "List"));
    ("import Array", (dummy, Import ("Array", None)));
    ("import Array as A", (dummy, Import ("Array", Some "A")));
    ("Array.length", (dummy, IdentMod ("Array", (dummy, Ident "length"))));
    ("(1)", (dummy, IntLit 1));
    ("(1,2)", (dummy, Tuple [(dummy, IntLit 1); (dummy, IntLit 2)]));
    ("(1,2,3)", (dummy, Tuple [(dummy, IntLit 1); (dummy, IntLit 2); (dummy, IntLit 3)]));


    ("type integer = int", (dummy, TypeDecl ("integer", [], TConstr (([], "int"), None))));
    ("type c = char and s = string",
        (dummy, TypeDeclAnd [(dummy, TypeDecl ("c", [], TConstr (([], "char"), None)));
            (dummy, TypeDecl ("s", [], TConstr (([], "string"), None)))]));
    ("type x = char and y = string and z = int",
        (dummy, TypeDeclAnd [(dummy, TypeDecl ("x", [], TConstr (([], "char"), None)));
            (dummy, TypeDecl ("y", [], TConstr (([], "string"), None)));
                (dummy, TypeDecl ("z", [], TConstr (([], "int"), None)))]));
    ("type f = (unit -> int)",
        (dummy, TypeDecl ("f", [], TFun (TConstr (([], "unit"), None), TConstr (([], "int"), None)))));
    ("type f = int -> int -> int)",
        (dummy, TypeDecl ("f", [], TFun (TConstr (([], "int"), None),
            TFun (TConstr (([], "int"), None), TConstr (([], "int"), None))))));
    ("type 'a f = ('a -> ('a -> 'a))",
        (dummy, TypeDecl ("f", [0], TFun (TVar (0, ref None), TFun (TVar (0, ref None), TVar (0, ref None))))));
    ("type 'a x = 'a", (dummy, TypeDecl ("x", [0], TVar (0, ref None))));
    ("type t = (int * char)",
        (dummy, TypeDecl ("t", [], TTuple [TConstr (([], "int"), None); TConstr (([], "char"), None)])));
    ("type f = float",
        (dummy, TypeDecl ("f", [], TConstr (([], "float"), None))));
    ("type l = (int list)",
        (dummy, TypeDecl ("l", [], TConstr (([], "list"), Some (TConstr (([], "int"), None))))));
    ("type 'a r = ('a ref)",
        (dummy, TypeDecl ("r", [0], TConstr (([], "ref"), Some (TVar (0, ref None))))));
    ("type 'a pair = ('a * 'a)",
        (dummy, TypeDecl ("pair", [0], TTuple [TVar (0, ref None); TVar (0, ref None)])));
    ("type  ('a, 'b) pair = ('a * 'b)",
        (dummy, TypeDecl ("pair", [0; 1], TTuple [TVar (0, ref None); TVar (1, ref None)])));
    ("type point2d = { mutable x : int; mutable y : int; }",
        (dummy, TypeDecl ("point2d", [], TRecord [("x", TConstr (([], "int"), None), Mutable);
            ("y", TConstr (([], "int"), None), Mutable)])));
    ("type 'a point2d = { x : 'a; y : 'a; }",
        (dummy, TypeDecl ("point2d", [0], TRecord [("x", TVar (0, ref None), Immutable);
            ("y", TVar (0, ref None), Immutable)])));
    ("type  ('a, 'b, 'c) atob = { a : 'a; b : 'b; c : 'c; }",
        (dummy, TypeDecl ("atob", [0; 1; 2], TRecord [("a", TVar (0, ref None), Immutable);
            ("b", TVar (1, ref None), Immutable); ("c", TVar (2, ref None), Immutable)])));
    ("type color =  | Red | Green | Blue",
        (dummy, TypeDecl ("color", [], TVariant [("Red", None); ("Green", None); ("Blue", None)])));
    ("type color =  | Red | Green | Blue",
        (dummy, TypeDecl ("color", [], TVariant [("Red", None); ("Green", None); ("Blue", None)])));
    ("type color =  | Red | Green | Blue | RGB (int * int * int)",
        (dummy, TypeDecl ("color", [], TVariant [("Red", None); ("Green", None); ("Blue", None);
            ("RGB", Some (TTuple [TConstr (([], "int"), None); TConstr (([], "int"), None); TConstr (([], "int"), None)]))])));
    ("type 'a option =  | None | Some 'a",
        (dummy, TypeDecl ("option", [0], TVariant [("None", None); ("Some", Some (TVar (0, ref None)))])));
    ("type 'a tree =  | Node 'a | Leaf (('a tree) * ('a tree))",
        (dummy, TypeDecl ("tree", [0], TVariant [("Node", Some (TVar (0, ref None)));
            ("Leaf", Some (TTuple [TConstr (([], "tree"), Some (TVar (0, ref None)));
                TConstr (([], "tree"), Some (TVar (0, ref None)))]))])));
    ("type itree = (int tree)", (dummy, TypeDecl ("itree", [], TConstr (([], "tree"), Some (TConstr (([], "int"), None))))));
    ("type lt = List.t", (dummy, TypeDecl ("lt", [], TConstr ((["List"], "t"), None))));
]

let parser_test verbose =
    print_string "Parser Test:";
    let do_parse (text, expected) =
        try
            if verbose then 
                print_endline ("text    > " ^ text)
            else ();
            let expr = Parser.parse_one @@ Scanner.from_string "TEST" text in
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
    ("fst (1,2)", VInt 1);
    ("snd (1,2)", VInt 2);
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

let eval_test verbose =
    print_string "Eval Test:";
    let do_eval (text, expected) =
        try
            if verbose then
                print_endline ("text> " ^ text)
            else ();
            let expr = Parser.parse_one @@ Scanner.from_string "TEST" text in
            let (new_env, v) = Eval.eval_decl (Symbol.get_current_env ()) expr in
            Symbol.set_current_env new_env;
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

let test () =
    scanner_test !g_verbose;
    parser_test !g_verbose;
    eval_test !g_verbose;
    test_report ()

