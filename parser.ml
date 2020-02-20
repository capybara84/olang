
open Syntax

type t = {
    scanner : Scanner.t;
    mutable token : token_t;
}

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


let new_parser scan = { scanner = scan; token = Scanner.get_token scan }

let peek_token pars = pars.token.token

let next_token pars =
    pars.token <- Scanner.get_token pars.scanner;
    debug_token @@ "token = " ^ token_to_string @@ peek_token pars

let rec parse_logical pars =
    Unit

and parse_let pars =
    Unit

and parse_fun pars =
    Unit

and parse_fn pars =
    Unit

and parse_if pars =
    Unit

and parse_match pars =
    Unit

and parse_compound pars =
    Unit

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
    
