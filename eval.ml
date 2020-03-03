
open Syntax

let error p msg = raise (Error (get_position_string p ^ ": Runtime Error: " ^ msg))
let warning p msg = print_endline @@ get_position_string p ^ ": Warning: " ^ msg


(*TODO Think if type errors really happen *)
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
    | (x, y) -> error n ("type error (equal " ^ value_to_string x ^ " != " ^ value_to_string y ^ ")")
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
(*
    if !g_verbose then print_endline @@ "eval: " ^ expr_to_string (n, e);
*)
    let res =
        match e with
        | Eof | Unit -> VUnit
        | Null -> VNull
        | WildCard -> VUnit (*TODO*)
        | BoolLit b -> VBool b
        | IntLit i -> VInt i
        | CharLit c -> VChar c
        | FloatLit f -> VFloat f
        | StringLit s -> VString s
        | Ident id | CIdent id -> 
            let v =
                (try
                    !(Env.lookup id env)
                with Not_found ->
                    (try
                        !(Symbol.lookup_default id)
                    with Not_found -> error n ("'" ^ id ^ "' not found")))
            in
            v
        | IdentMod (id, e) ->
            (try
                let tab = Symbol.lookup_module id in
                eval tab.env e
            with Not_found -> error n ("'" ^ id ^ "' not found"))
        | Record (e, id) ->
            VUnit (*TODO implement *)
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
            let (_, v) = eval_list false env el in
            v
        | _ -> failwith ("eval bug :" ^ expr_to_string (n, e) )
    in
(*
    if !g_verbose then print_endline @@ "eval: " ^ expr_to_string (n, e) ^ " = " ^ value_to_string res;
*)
    res


and eval_list is_toplevel env el =
    match el with
    | [] -> (env, VUnit)
    | x::xs ->
        if is_toplevel && !g_output_source then begin
            print_endline @@ "(\"" ^ expr_to_string x ^ "\", " ^ expr_to_string_src x ^ ");";
            eval_list is_toplevel env xs
        end else
            let (new_env, v) = eval_decl env x in
            if xs == [] then
                (new_env, v)
            else if v <> VUnit then
                (warning (fst x) "Type unit required";
                eval_list is_toplevel new_env xs)
            else
                eval_list is_toplevel new_env xs

and eval_decl env x =
(*
    if !g_verbose then print_endline @@ "eval_decl: " ^ expr_to_string_src x;
*)
    match x with
    | (_, Comp []) ->
        (env, VUnit)
    | (_, Comp el) ->
        let (env, v) = eval_list true env el in
        (env, v)
    | (_, Ident "#env") ->
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
        (*TODO env *)
        List.iter (fun x -> ignore @@ type_decl env x) el;
        (env, VUnit)
    | (_, Module id) ->
        let tab = Symbol.set_module id in
        (tab.env, VUnit)
    | (_, Import _) ->
        (env, VUnit)
    | e ->
        (env, eval env e)

and type_decl env e =
    match e with
    | (p, TypeDecl (id, tvl, t)) ->
        Symbol.insert_type_if_variant id t;
        (env, VUnit)
    | _ -> failwith "type decl bug"

and eval_top e =
    let tab = Symbol.get_current_module () in
    let (env, v) = eval_decl tab.env e in
    Symbol.set_current_env env;
    v

