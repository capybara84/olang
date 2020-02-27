open Syntax

let make_type id = TConstr (([], id), None)

let t_unit = make_type "unit"
let t_bool = make_type "bool"
let t_int = make_type "int"
let t_char = make_type "char"
let t_float = make_type "float"
let t_string = make_type "string"

(*
    ref TConstr(([], "ref"), Some (TVar (0, ref None)))
*)

let new_list t =
    TConstr (([], "list"), Some t)

let new_tuple tl =
    TTuple tl

