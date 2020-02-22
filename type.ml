open Syntax

let unit_t = Tycon ("unit", [])
let bool_t = Tycon ("bool", [])
let int_t = Tycon ("int", [])
let char_t = Tycon ("char", [])
let float_t = Tycon ("float", [])
let string_t = Tycon ("string", [])
let make_tuple_t tl = Tycon ("*", tl)
let make_func_t t1 t2 = Tycon ("->", [t1;t2])
let make_list_t t = Tycon ("list", [t])
let make_ref_t t = Tycon ("&", [t])


