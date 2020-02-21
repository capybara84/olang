
open Syntax

let error n msg = raise (Error (string_of_int n ^ ": Runtime Error: " ^ msg))


let eval_unary _ = VUnit
let eval_binary _ = VUnit

let rec eval env (n, e) =
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
            error n ("'" ^ id ^ "' not found"))
        (*
            (try
                !(lookup_default id)
            with Not_found -> error n ("'" ^ id ^ "' not found")))
        *)
    | IdentMod (id, e) ->
        VUnit (*TODO*)
    | Record (e, id) ->
        VUnit (*TODO*)
    | Tuple el ->
        VTuple (List.map (eval env) el)
    | Binary (BinLor, lhs, rhs) ->
    (*
        type_check TBool lhs n;
        type_check TBool rhs n;
    *)
        if eval env lhs = VBool true then
            VBool true
        else
            eval env rhs
    | Binary (BinLand, lhs, rhs) ->
    (*
        type_check TBool lhs n;
        type_check TBool rhs n;
    *)
        if eval env lhs = VBool false then
            VBool false
        else
            eval env rhs
    | Binary (op, lhs, rhs) ->
        eval_binary (op, eval env lhs, eval env rhs)
    | Unary (op, e) ->
        eval_unary (op, eval env e)
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

and eval_decl env = function
    | (_, Let (id, e)) ->
        let v = eval env e in
        let new_env = Env.extend id (ref v) env in
        (new_env, VUnit)
    | (_, LetRec (id, e)) ->
        let r = ref VUnit in
        let new_env = Env.extend id r env in
        r := eval new_env e;
        (new_env, VUnit)
    | (_, Module id) ->
        (env, VUnit) (*TODO*)
    | (_, Import (id, None)) ->
        (env, VUnit) (*TODO*)
    | (_, Import (id, Some aid)) ->
        (env, VUnit) (*TODO*)
    | e ->
        (env, eval env e)

