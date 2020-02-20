
open Syntax

type t = {
    scanner : Scanner.t;
    mutable token : token_t;
}

let get_source_position pars =
    Scanner.get_source_position pars.scanner

let debug_scope_flag = ref false
let debug_token_flag = ref false
let debug_indent = ref 0

let rec debug_show_space n =
    if n = 0 then ()
    else begin
        print_char ' ';
        debug_show_space (n-1)
    end
let debug_print_indent s =
    if !debug_scope_flag then begin
        debug_show_space !debug_indent;
        print_endline s
    end
let debug_parse_in s =
    if !debug_scope_flag then begin
        debug_show_space !debug_indent;
        incr debug_indent;
        print_endline ("IN " ^ s)
    end
let debug_parse_out s =
    if !debug_scope_flag then begin
        decr debug_indent;
        debug_show_space !debug_indent;
        print_endline ("OUT " ^ s)
    end

let debug_parse msg = debug_print_indent msg
let debug_token msg =
    if !debug_token_flag then
        print_endline msg


let error pars msg =
    raise (Error (get_source_position pars ^ ": " ^ msg))

let new_parser scan = { scanner = scan; token = Scanner.get_token scan }

let peek_token pars = pars.token.token

let next_token pars =
    pars.token <- Scanner.get_token pars.scanner;
    debug_token @@ "token = " ^ token_to_string @@ peek_token pars

let rec expect pars token =
    let current_token = peek_token pars in
    if current_token = token then
        next_token pars
    else if current_token = NEWLINE then
        (next_token pars; expect pars token)
    else
        error pars ("missing token '" ^ token_to_string token ^
                "' at '" ^ token_to_string current_token ^ "'")

let rec expect_id pars =
    match peek_token pars with
    | ID id -> next_token pars; id
    | NEWLINE -> next_token pars; expect_id pars
    | t -> error pars ("missing identifier at '" ^ token_to_string t ^ "'")

let rec expect_c_id pars =
    match peek_token pars with
    | C_ID id -> next_token pars; id
    | NEWLINE -> next_token pars; expect_c_id pars
    | t -> error pars ("missing capitalized identifier at '" ^ token_to_string t ^ "'")

let rec skip_newline pars =
    match peek_token pars with
    | NEWLINE ->
        next_token pars; skip_newline pars
    | _ -> ()

let rec parse_logical pars =
    next_token pars;
    Unit    (*TODO*)

and parse_let pars =
    next_token pars;
    Unit    (*TODO*)

and parse_fun pars =
    next_token pars;
    Unit    (*TODO*)

and parse_param_list pars args =
    debug_parse_in "parse_param_list";
    let e =
        match peek_token pars with
        | WILDCARD ->
            next_token pars;
            parse_param_list pars (WildCard::args)
        | ID id ->
            next_token pars;
            parse_param_list pars (Ident id::args)
        | _ ->
            List.rev args
    in
    debug_parse_out "parse_param_list";
    e

and parse_params pars =
    debug_parse_in "parse_params pars";
    let e =
        if peek_token pars = UNIT then
            (next_token pars; [Unit])
        else
            parse_param_list pars []
    in
    debug_parse_out "parse_params pars";
    e

and parse_fn pars =
    debug_parse_in "parse_fn";
    next_token pars;
    skip_newline pars;
    let args = parse_params pars in
    expect pars RARROW;
    skip_newline pars;
    let e = List.fold_right (fun arg body -> Fn (arg, body)) args (parse_expr pars) in
    debug_parse_out "parse_fn";
    e

and parse_if pars =
    debug_parse_in "parse_if";
    next_token pars;
    skip_newline pars;
    let e1 = parse_expr pars in
    expect pars THEN;
    skip_newline pars;
    let e2 = parse_expr pars in
    let e3 =
        if peek_token pars = ELSE then begin
            next_token pars;
            skip_newline pars;
            parse_expr pars
        end else Unit
    in
    debug_parse_out "parse_if";
    If (e1, e2, e3)

and parse_match pars =
    next_token pars;
    Unit    (*TODO*)

and parse_expr_list pars =
    debug_parse_in "parse_expr_list";
    let rec loop () =
        match peek_token pars with
            | EOF | RBRACE -> []
            | _ ->
                begin
                    let e = parse_expr pars in
                    skip_newline pars;
                    e::(loop ())
                end
    in
    let e = loop () in
    debug_parse_in "parse_expr_list";
    e

and parse_compound pars =
    debug_parse_in "parse_compound";
    next_token pars;
    skip_newline pars;
    let e = parse_expr_list pars in
    expect pars RBRACE;
    skip_newline pars;
    debug_parse_out "parse_compound";
    Comp e

and parse_expr pars =
    debug_parse_in "parse_expr";
    let e =
        match peek_token pars with
        | EOF -> Eof
        | NEWLINE | SEMI -> next_token pars; parse_expr pars
        | LET -> parse_let pars
        | FUN -> parse_fun pars
        | FN -> parse_fn pars
        | IF -> parse_if pars
        | MATCH -> parse_match pars
        | LBRACE -> parse_compound pars
        | _ -> parse_logical pars
    in
    if peek_token pars = SEMI then
        next_token pars;
    debug_parse_out "parse_expr";
    e

let parse_type_def pars =
    next_token pars;
    Unit    (*TODO*)

let parse_module pars =
    next_token pars;
    Unit    (*TODO*)

let parse_import pars =
    next_token pars;
    Unit    (*TODO*)

let rec parse_decl pars =
    debug_parse_in "parse_decl";
    let e =
        match peek_token pars with
        | EOF -> Eof
        | NEWLINE | SEMI -> next_token pars; parse_decl pars
        | MODULE -> parse_module pars
        | IMPORT -> parse_import pars
        | TYPE -> parse_type_def pars
        | _ -> parse_expr pars
    in
    debug_parse_out "parse_decl";
    e

let parse scanner =
    let pars = new_parser scanner in
    parse_decl pars
    
