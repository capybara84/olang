
open Syntax

let error s = raise (Error ("Runtime error: " ^ s))
let type_error s = error ("type '" ^ s ^ "' required")

let fn_nl _ =
    print_newline ();
    flush stdout;
    VUnit

let fn_putn = function
    | VInt n -> print_int n; VUnit
    | _ -> type_error "int"

let fn_putc = function
    | VChar c -> print_char c; VUnit
    | _ -> type_error "char"

let fn_putf = function
    | VFloat f -> print_float f; VUnit
    | _ -> type_error "float"

let fn_puts = function
    | VString s -> print_string s; VUnit
    | _ -> type_error "string"

let fn_head = function
    | VCons (hd, _) -> hd
    | _ -> type_error "list"

let fn_tail = function
    | VCons (_, tl) -> tl
    | _ -> type_error "list"

let fn_first = function
    | VTuple (x::_) -> x
    | _ -> type_error "tuple"

let fn_second = function
    | VTuple (_::x::_) -> x
    | _ -> type_error "tuple"

let fn_env _ =
    print_endline "Env";
    VUnit

let builtin_list =
    [
        ("true", VBool true);
        ("false", VBool false);
        ("nl", VBuiltin fn_nl);
        ("putn", VBuiltin fn_putn);
        ("putc", VBuiltin fn_putc);
        ("putf", VBuiltin fn_putf);
        ("puts", VBuiltin fn_puts);
        ("hd", VBuiltin fn_head);
        ("tl", VBuiltin fn_tail);
        ("fst", VBuiltin fn_first);
        ("snd", VBuiltin fn_second);
        ("env", VBuiltin fn_env);
    ]

let init () =
    List.iter (fun (name, value) -> Symbol.insert_default name value) builtin_list

