
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

let builtin_list =
    let hd_type = TVar (0, ref None) in
    let tl_type = TVar (0, ref None) in
    let fst_type = TVar (0, ref None) in
    let fst'_type = TVar (1, ref None) in
    let snd_type = TVar (0, ref None) in
    let snd'_type = TVar (1, ref None) in
    [
        ("true", Type.t_bool, VBool true);
        ("false", Type.t_bool, VBool false);
        ("nl", TFun (Type.t_unit, Type.t_unit), VBuiltin fn_nl);
        ("putn", TFun (Type.t_int, Type.t_unit), VBuiltin fn_putn);
        ("putc", TFun (Type.t_char, Type.t_unit), VBuiltin fn_putc);
        ("putf", TFun (Type.t_float, Type.t_unit), VBuiltin fn_putf);
        ("puts", TFun (Type.t_string, Type.t_unit), VBuiltin fn_puts);
        ("hd", TFun (Type.new_list hd_type, hd_type), VBuiltin fn_head);
        ("tl", TFun (Type.new_list tl_type, Type.new_list tl_type), VBuiltin fn_tail);
        ("fst", TFun (Type.new_tuple [fst_type; fst'_type], fst_type), VBuiltin fn_first);
        ("snd", TFun (Type.new_tuple [snd_type; snd'_type], snd'_type), VBuiltin fn_second);
    ]

let init () =
    List.iter (fun (name, ty, value) -> Symbol.insert_default name ty value) builtin_list

