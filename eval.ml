
open Syntax

let error n msg = raise (Error (string_of_int n ^ ": Runtime Error: " ^ msg))

let default_directory = "./"
let default_extension = ".ol"

let make_module_filename name =
    default_directory ^ String.uncapitalize name ^ default_extension

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let eval_unary n = function
    | (UMinus, VInt i) -> VInt (-i)
    | (UMinus, VFloat f) -> VFloat (-.f)
    | (UNot, VBool b) -> VBool (not b)
    | _ -> error n "type error (unary expression)"

let rec eval_equal n = function
    | (VUnit, VUnit) | (VNull, VNull) -> true
    | (VBool x, VBool y) -> (x = y)
    | (VInt x, VInt y) -> (x = y)
    | (VChar x, VChar y) -> (x = y)
    | (VFloat x, VFloat y) -> (x = y)
    | (VString x, VString y) -> (x = y)
    | (VTuple xl, VTuple yl) -> equal_list n (xl, yl)
    | ((VCons _ as x), (VCons _ as y)) -> equal_cons n (x, y)
    | (VCons _, VNull) | (VNull, VCons _) -> false
    | _ -> error n "type error (equal)"
and equal_list n = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if not (eval_equal n (x, y)) then false
        else equal_list n (xs, ys)
and equal_cons n = function
    | (VNull, VNull) -> true
    | (VCons _, VNull) | (VNull, VCons _) -> false
    | (VCons (x, xs), VCons (y, ys)) ->
        if not (eval_equal n (x, y)) then false
        else equal_cons n (xs, ys)
    | _ -> error n "type error (equal cons)"


let eval_binary n = function
    | (BinEql, x, y) -> VBool (eval_equal n (x, y))
    | (BinNeq, x, y) -> VBool (not (eval_equal n (x, y)))
    | (BinAdd, VInt x, VInt y) -> VInt (x + y)
    | (BinSub, VInt x, VInt y) -> VInt (x - y)
    | (BinMul, VInt x, VInt y) -> VInt (x * y)
    | (BinDiv, VInt x, VInt y) -> VInt (x / y)
    | (BinMod, VInt x, VInt y) -> VInt (x mod y)
    | (BinLT, VInt x, VInt y) -> VBool (x < y)
    | (BinLE, VInt x, VInt y) -> VBool (x <= y)
    | (BinGT, VInt x, VInt y) -> VBool (x > y)
    | (BinGE, VInt x, VInt y) -> VBool (x >= y)
    | (BinLT, VChar x, VChar y) -> VBool (x < y)
    | (BinLE, VChar x, VChar y) -> VBool (x <= y)
    | (BinGT, VChar x, VChar y) -> VBool (x > y)
    | (BinGE, VChar x, VChar y) -> VBool (x >= y)
    | (BinAdd, VFloat x, VFloat y) -> VFloat (x +. y)
    | (BinSub, VFloat x, VFloat y) -> VFloat (x -. y)
    | (BinMul, VFloat x, VFloat y) -> VFloat (x *. y)
    | (BinDiv, VFloat x, VFloat y) -> VFloat (x /. y)
    | (BinLT, VFloat x, VFloat y) -> VBool (x < y)
    | (BinLE, VFloat x, VFloat y) -> VBool (x <= y)
    | (BinGT, VFloat x, VFloat y) -> VBool (x > y)
    | (BinGE, VFloat x, VFloat y) -> VBool (x >= y)
    | (BinAdd, VString x, VString y) -> VString (x ^ y)
    | (BinLT, VString x, VString y) -> VBool (x < y)
    | (BinLE, VString x, VString y) -> VBool (x <= y)
    | (BinGT, VString x, VString y) -> VBool (x > y)
    | (BinGE, VString x, VString y) -> VBool (x >= y)
    | (BinCons, x, y) -> VCons (x, y)
    | _ -> error n "type error (binary expression)"

let rec eval env (n, e) =
    if !g_verbose then print_endline @@ "eval: " ^ expr_to_string (n, e);
    match e with
    | Eof | Unit -> VUnit
    | Null -> VNull
    | WildCard -> VUnit (*TODO*)
    | BoolLit b -> VBool b
    | IntLit i -> VInt i
    | CharLit c -> VChar c
    | FloatLit f -> VFloat f
    | StringLit s -> VString s
    | Ident id ->
        (try
            !(Env.lookup id env)
        with Not_found ->
            (try
                !(Symbol.lookup_default id)
            with Not_found -> error n ("'" ^ id ^ "' not found")))
    | IdentMod (id, e) ->
        (try
            let tab = Symbol.lookup_module id in
            eval tab.env e
        with Not_found -> error n ("'" ^ id ^ "' not found"))
    | Record (e, id) ->
        VUnit (*TODO*)
    | Tuple el ->
        VTuple (List.map (eval env) el)
    | Binary (BinLor, lhs, rhs) ->
        if eval env lhs = VBool true then
            VBool true
        else
            eval env rhs
    | Binary (BinLand, lhs, rhs) ->
        if eval env lhs = VBool false then
            VBool false
        else
            eval env rhs
    | Binary (op, lhs, rhs) ->
        eval_binary n (op, eval env lhs, eval env rhs)
    | Unary (op, e) ->
        eval_unary n (op, eval env e)
    | Fn (arg, body) ->
        VClosure (arg, body, env)
    | Apply (fn, arg) ->
        let closure = eval env fn in
        let arg_value = eval env arg in
        (match closure with
            | VClosure ((_, Ident carg), body, c_env) ->
                let new_env = Env.extend carg (ref arg_value) c_env in
                eval new_env body
            | VClosure ((_, WildCard), body, c_env)
            | VClosure ((_, Unit), body, c_env) ->
                eval c_env body
            | VBuiltin fn ->
                fn arg_value
            | v -> error n ("application of non-function: " ^ value_to_string v))
    | If (cond_e, then_e, else_e) ->
        (match eval env cond_e with
            | VBool true -> eval env then_e
            | VBool false -> eval env else_e
            | _ -> error n "bool expected")
    | Comp el ->
        let (_, v) = eval_list env el in
        v
    | _ -> failwith "eval bug"

and eval_list env el =
    match el with
    | [] -> (env, VUnit)
    | x::xs ->
        let (new_env, v) = eval_decl env x in
        if xs == [] then
            (new_env, v)
        else if v <> VUnit then
            error (fst x) ("unit required")
        else
            eval_list new_env xs

and eval_decl env x =
    if !g_verbose then print_endline @@ "eval_decl: " ^ expr_to_string x;
    match x with
    | (_, Ident "env") ->
        Symbol.show_all_modules ();
        (env, VUnit)
    | (_, Let (id, e)) ->
        let v = eval env e in
        let new_env = Env.extend id (ref v) env in
        (new_env, VUnit)
    | (_, LetRec (id, e)) ->
        let r = ref VUnit in
        let new_env = Env.extend id r env in
        r := eval new_env e;
        (new_env, VUnit)
    | (_, TypeDecl _) ->
        type_decl env x
    | (_, TypeDeclAnd el) ->
        List.iter (fun x -> ignore @@ type_decl env x) el;
        (env, VUnit)
    | (_, Module id) ->
        let tab = Symbol.set_module id in
        (tab.env, VUnit)
    | (_, Import (id, None)) ->
        import id;
        (env, VUnit)
    | (_, Import (id, Some aid)) ->
        import id;
        Symbol.rename_module id aid;
        (env, VUnit)
    | e ->
        (env, eval env e)

and type_decl env e =
    match e with
    | (n, TypeDecl (id, tvl, t)) ->
        (env, VUnit)    (* TODO *)
    | _ -> failwith "type decl bug"

and import id =
    if Symbol.exist_module id then
        ()
    else begin
        let filename = make_module_filename id in
        let prev = Symbol.get_current_module () in
        ignore (Symbol.set_module id);
        (try
            load_source filename
        with Error s | Sys_error s -> print_endline s);
        Symbol.set_current_module prev
    end

and load_source filename =
    try
        let text = load_file filename in
        let scan = Scanner.from_string filename text in
        let rec loop () =
            let e = Parser.parse scan in
            match e with
            | (_, Eof) -> ()
            | _ ->
                if !g_verbose then
                    print_endline @@ expr_to_string e;
                if !g_output_source then
                    print_endline @@ "(\"" ^ expr_to_string e ^ "\", " ^ expr_to_string_src e ^ ");"
                else begin
                    let v = eval_top e in
                    if v <> VUnit then
                        print_endline "Warning: The expression should have type unit"
                end;
                loop ()
        in loop ()
    with Error s | Sys_error s -> print_endline s

and eval_top e =
    let (env, v) = eval_decl (Symbol.get_current_env ()) e in
    Symbol.set_current_env env;
    v


