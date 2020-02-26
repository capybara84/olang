open Syntax

let make_type id = TConstr (([], id), None)

let t_unit = make_type "unit"
let t_bool = make_type "bool"
let t_int = make_type "int"
let t_char = make_type "char"
let t_float = make_type "float"
let t_string = make_type "string"
(*
    list TConstr(([0], "list"), Some (TVar (0, ref None)))
    ref TConstr(([0], "ref"), Some (TVar (0, ref None)))
*)
