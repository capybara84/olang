open Syntax

val insert_module : string -> symtab
val lookup_module : string -> symtab
val exist_module : string -> bool
val get_current_module : unit -> symtab
val get_current_module_name : unit -> string
val set_current_module : symtab -> unit

val set_module : string -> symtab
val set_default_module : unit -> unit
val rename_module : string -> string -> unit


val get_current_env : unit -> value ref Env.t
val set_current_env : value ref Env.t -> unit
val get_current_tenv : unit -> typ ref Env.t
val set_current_tenv : typ ref Env.t -> unit

val lookup_default : string -> value ref
val lookup_default_type : string -> typ ref

val insert_default : string -> typ -> value -> unit
val insert_default_type : string -> typ -> unit

val insert_type : string -> typ -> unit
val insert_type_if_variant : string -> typ -> unit

val show_tab : symtab -> unit
val show_all_modules : unit -> unit

