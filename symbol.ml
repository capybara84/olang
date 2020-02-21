open Syntax

let default_module_name = "Main"

let all_modules = ref Env.empty

let insert_module id =
    let tab = { env = Env.empty } in
    all_modules := Env.extend id tab !all_modules;
    tab

let default_module = insert_module default_module_name
let current_module = ref default_module

let get_current_env () = !current_module.env
let set_current_env env = !current_module.env <- env

let set_default_module () =
    current_module := default_module

let insert_default name value =
    default_module.env <- Env.extend name (ref value) default_module.env

let show_env env =
    List.iter (fun (name, value) ->
        print_endline @@ " " ^ name ^ " = " ^ value_to_string !value) env

let show_all_modules () =
    List.iter (fun (name, tab) ->
        print_endline @@ "Module " ^ name;
        show_env tab.env) !all_modules

