open Syntax

let default_module_name = "Main"

let all_modules = ref Env.empty

let insert_module id =
    let tab = { env = Env.empty } in
    all_modules := Env.extend id tab !all_modules;
    tab

let lookup_module id =
    Env.lookup id !all_modules

let exist_module id =
    try ignore (Env.lookup id !all_modules); true
    with Not_found -> false

let default_module = insert_module default_module_name
let current_module = ref default_module

let get_current_module () = !current_module
let set_current_module tab = current_module := tab

let set_module id =
    (try
        current_module := lookup_module id
    with Not_found ->
        current_module := insert_module id);
    !current_module


let get_current_env () = !current_module.env
let set_current_env env = !current_module.env <- env

let set_default_module () =
    current_module := default_module

let lookup_default id =
    Env.lookup id default_module.env

let insert_default name value =
    default_module.env <- Env.extend name (ref value) default_module.env

let rename_module old_name new_name =
    let tab = lookup_module old_name in
    let module_list = List.remove_assoc old_name !all_modules in
    all_modules := Env.extend new_name tab module_list


let show_env env =
    List.iter (fun (name, value) ->
        print_endline @@ " " ^ name ^ " = " ^ value_to_string !value) env

let show_all_modules () =
    List.iter (fun (name, tab) ->
        print_endline @@ "Module " ^ name;
        show_env tab.env) !all_modules

