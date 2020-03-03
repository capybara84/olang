open Syntax

let default_module_name = "Main"

let all_modules = ref Env.empty

let insert_module id =
    let tab = { env = Env.empty; tenv = Env.empty; module_name = id } in
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
let get_current_module_name () = !current_module.module_name
let set_current_module tab = current_module := tab

let set_module id =
    (try
        current_module := lookup_module id
    with Not_found ->
        current_module := insert_module id);
    !current_module

let set_default_module () =
    current_module := default_module

let rename_module old_name new_name =
    let tab = lookup_module old_name in
    let module_list = List.remove_assoc old_name !all_modules in
    all_modules := Env.extend new_name tab module_list



let get_current_env () = !current_module.env
let set_current_env env = !current_module.env <- env
let get_current_tenv () = !current_module.tenv
let set_current_tenv tenv = !current_module.tenv <- tenv


let lookup_default id =
    Env.lookup id default_module.env
let lookup_default_type id =
    Env.lookup id default_module.tenv

let insert_default name ty value =
    default_module.env <- Env.extend name (ref value) default_module.env;
    default_module.tenv <- Env.extend name (ref ty) default_module.tenv

let insert_default_type name ty =
    default_module.tenv <- Env.extend name (ref ty) default_module.tenv

let insert_type id ty =
    let id_ty = TId (id, ty) in
    !current_module.tenv <- Env.extend id (ref id_ty) !current_module.tenv;
    let rec insert_variant n = function
        | [] -> ()
        | x::xs -> insert_variant_elem n x; insert_variant (n+1) xs
    and insert_variant_elem n = function
        | (id, _) ->
            !current_module.tenv <- Env.extend id (ref id_ty) !current_module.tenv 
    in
    match ty with
    | TVariant vl ->
        insert_variant 0 vl 
    | _ -> ()


let rec insert_type_if_variant _ t =
    let rec insert_variant n = function
        | [] -> ()
        | x::xs -> insert_variant_elem n x; insert_variant (n+1) xs
    and insert_variant_elem n = function
        | (id, _) ->
            !current_module.env <- Env.extend id (ref (VInt n)) !current_module.env 
    in
    match t with
    | TVariant vl ->
        insert_variant 0 vl 
    | _ -> ()

let show_tab tab =
    print_endline "env";
    List.iter (fun (name, value) ->
        print_endline @@ " " ^ name ^ " = " ^ value_to_string !value) tab.env;
    print_endline "tenv";
    List.iter (fun (name, ty) ->
        print_endline @@ " " ^ name ^ " = " ^ type_to_string !ty) tab.tenv

let show_all_modules () =
    List.iter (fun (name, tab) ->
        print_endline @@ "Module " ^ name; show_tab tab) !all_modules

