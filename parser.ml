
open Syntax

type t = {
    scanner : Scanner.t;
    mutable token : token_t;
}

let new_parser scan = { scanner = scan; token = Scanner.get_token scan }

let peek_token pars = pars.token.token

let next_token pars = pars.token <- Scanner.get_token pars.scanner

let parse_expr pars =
    Unit

let parse_type_def pars =
    Unit

let parse_module pars =
    Module "@"

let parse_import pars =
    Import ("@", None)

let rec parse_program pars =
    match peek_token pars with
    | EOF -> Eof
    | NEWLINE | SEMI -> next_token pars; parse_program pars
    | MODULE -> parse_module pars
    | IMPORT -> parse_import pars
    | TYPE -> parse_type_def pars
    | _ -> parse_expr pars

let parse scanner =
    let pars = new_parser scanner in
    parse_program pars
    
