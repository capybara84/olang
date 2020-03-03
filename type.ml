open Syntax

let error p msg = raise (Error (get_position_string p ^ ": Type Error: " ^ msg))

let verbose msg =
    if !g_verbose_type then
        print_endline ("TYPE> " ^ msg)

let seed = ref 0

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

let t_ref = TConstr (([], "ref"), Some (TVar (0, ref None)))
let t_empty_list = TConstr (([], "list"), Some (TVar (0, ref None)))

let new_tvar  () =
    let ty = TVar (!seed, ref None) in
    incr seed;
    ty

let t_id id ty =
    TId (id, ty)

let t_list t =
    TConstr (([], "list"), Some t)

let t_tuple tl =
    TTuple tl

let rec list_equal l1 l2 =
    match (l1, l2) with
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) -> x = y && list_equal xs ys

let type_name_equal (path1, id1) (path2, id2) =
    list_equal path1 path2 && id1 = id2

let rec equal t1 t2 =
    match (t1, t2) with
    | (TConstr (tn1, None), TConstr (tn2, None)) when type_name_equal tn1 tn2 -> true
    | (TTuple tl1, TTuple tl2) -> list_equal tl1 tl2
    | (TFun (t11, t12), TFun (t21, t22)) -> equal t11 t21 && equal t12 t22
    | (TVar (n1, {contents = None}), TVar (n2, {contents = None})) -> n1 = n2
    | (TVar (_, {contents = None}), _) | (_, TVar (_, {contents = None})) -> true
    | (TVar (_, {contents = Some t1'}), _) -> equal t1' t2
    | (_, TVar (_, {contents = Some t2'})) -> equal t1 t2'
    | _ -> false

let rec type_var_equal t1 t2 =
    match (t1, t2) with
    | (TVar (n, {contents = None}), TVar (m, {contents = None})) when n = m
        -> true
    | (TVar (_, {contents = Some t1'}), _)
        -> type_var_equal t1' t2
    | (_, TVar (_, {contents = Some t2'}))
        -> type_var_equal t1 t2'
    | _ -> false

let rec occurs_in t tl =
    List.exists (fun t2 -> occurs_in_type t t2) tl

and occurs_in_type t t2 =
    (*TODO prune ? *)
    if type_var_equal t t2 then true
    else
        match t2 with
        | TTuple tl -> occurs_in t tl
        | TFun (tf1, tf2) -> occurs_in t [tf1;tf2]
        | _ -> false

let rec prune = function
    | TVar (_, ({contents = Some t} as instance)) ->
        let inst = prune t in
        instance := Some inst;
        inst
    | t -> t

let rec unify p t1 t2 =
    let t1 = prune t1 in
    let t2 = prune t2 in
    verbose @@ "unify " ^ type_to_string_src t1 ^ ", " ^ type_to_string_src t2;
    match (t1, t2) with
    | (TConstr (tn1, None), TConstr (tn2, None)) when type_name_equal tn1 tn2 -> ()
    | (TConstr (tn1, Some tl), TConstr (tn2, Some tr)) when type_name_equal tn1 tn2 ->
        unify p tl tr
    | (TTuple tl1, TTuple tl2) ->
        (try
            List.iter2 (fun a b -> unify p a b) tl1 tl2
        with Invalid_argument _ -> error p @@ "type mismatch " ^ type_to_string t1 ^ " and " ^ type_to_string t2)
    | (TFun (t11, t12), TFun (t21, t22)) ->
        unify p t11 t21;
        unify p t12 t22
    | (TVar (n1, {contents = None}), TVar (n2, {contents = None})) when n1 = n2 -> ()
    | (TVar (_, {contents = Some t1'}), _) -> unify p t1' t2
    | (_, TVar (_, {contents = Some t2'})) -> unify p t1 t2'

    | (TVar (_, ({contents = None} as r1)), _) ->
        if occurs_in_type t1 t2 then
            error p @@ "type circularity between " ^ type_to_string t1 ^ " and " ^ type_to_string t2
        else
            r1 := Some t2
    | (_, TVar (_, ({contents = None} as r2))) ->
        if occurs_in_type t2 t1 then
            error p @@ "type circularity between " ^ type_to_string t2 ^ " and " ^ type_to_string t1
        else
            r2 := Some t1
    | (_, _) -> error p @@ "type mismatch " ^ type_to_string t1 ^ " & " ^ type_to_string t2


let infer_binary p op tl tr =
    match op with
    | BinAdd ->
        unify p tl tr;
        if equal tl t_int || equal tl t_string || equal tl t_float then
            tl
        else
            error p @@ "int, float, or string required for '+' operator (" ^ type_to_string tl ^ ")"
    | BinSub | BinMul | BinDiv | BinMod ->
        unify p tl tr;
        if equal tl t_int || equal tl t_float then
            tl
        else
            error p @@ "int or float required for '" ^ string_of_binop op ^ "' operator ("
                ^ type_to_string tl ^ ")"
    | BinLT | BinLE | BinGT | BinGE ->
        unify p tl tr;
        if equal tl t_char || equal tl t_int || equal tl t_string || equal tl t_float then
            t_bool
        else
            error p @@ "int, float, char or string required for relational operator ("
                ^ type_to_string tl ^ ")"
    | BinEql | BinNeq ->
        unify p tl tr;
        t_bool
    | BinLor | BinLand ->
        unify p t_bool tl;
        unify p t_bool tr;
        t_bool
    | BinCons ->
        unify p (t_list tl) tr;
        t_list tl

let infer_unary p op t =
    match (op, t) with
    | (UMinus, TConstr (([], "int"), None)) ->
        t_int
    | (UMinus, TConstr (([], "float"), None)) ->
        t_float
    | (UNot, TConstr (([], "bool"), None)) ->
        t_bool
    | _ -> error p @@ "unary operator " ^ string_of_unop op ^ " type error (" ^ type_to_string t ^ ")"

(* TODO Difference about Global Comp and Local Comp *)
let rec infer_list p tenv = function
    | [] -> (tenv, t_unit)
    | x::xs ->
        let (tenv, t) = infer tenv x in
        if xs = [] then
            (tenv, t)
        else begin
            unify p t_unit t;
            infer_list p tenv xs
        end

and infer tenv x =
    verbose @@ "infer  IN: " ^ expr_to_string x;
    let (tenv, ty) =
        match x with
        | (_, Eof) | (_, Unit) -> (tenv, t_unit)
        | (_, Null) -> (tenv, t_list @@ new_tvar ())
        | (_, WildCard) -> (tenv, new_tvar ())
        | (_, BoolLit b) -> (tenv, t_bool)
        | (_, IntLit i) -> (tenv, t_int)
        | (_, CharLit c) -> (tenv, t_char)
        | (_, FloatLit f) -> (tenv, t_float)
        | (_, StringLit s) -> (tenv, t_string)
        | (_, Ident "#env") ->
            (tenv, t_unit)
        | (p, Ident id) | (p, CIdent id) ->
            let t =
                (try
                    !(Env.lookup id tenv)
                with Not_found ->
                    (try
                        !(Symbol.lookup_default_type id)
                    with Not_found -> error p ("'" ^ id ^ "' not found")))
            in
            (tenv, t)
        | (p, IdentMod (id, e)) ->
            (try
                let tab = Symbol.lookup_module id in
                infer tab.tenv e
            with Not_found -> error p ("'" ^ id ^ "' not found"))
        | (_, Record (e, id)) ->
            (tenv, t_unit) (*TODO*)
        | (_, Tuple el) ->
            (tenv, t_tuple (List.map (fun x -> let (_, t) = infer tenv x in t) el))
        | (p, Binary (op, lhs, rhs)) ->
            let (_, tl) = infer tenv lhs in
            let (_, tr) = infer tenv rhs in
            let t = infer_binary p op tl tr in
            (tenv, t)
        | (p, Unary (op, e)) ->
            let (_, t) = infer tenv e in
            let t = infer_unary p op t in
            (tenv, t)
        | (_, Fn ((_, Ident id), body)) ->
            let t_arg = new_tvar () in
            let tenv = Env.extend id (ref t_arg) tenv in
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_arg, t_body))
        | (_, Fn ((_, WildCard), body)) ->
            let t_arg = new_tvar () in
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_arg, t_body))
        | (_, Fn ((_, Unit), body)) ->
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_unit, t_body))
        | (_, Fn _) -> failwith "type bug"
        | (p, Apply (fn, arg)) ->
            verbose "Apply";
            let (_, t_fn) = infer tenv fn in
            let (_, t_arg) = infer tenv arg in
            let t = new_tvar () in
            unify p t_fn (TFun (t_arg, t));
            (tenv, t)
        | (p, If (cond_e, then_e, else_e)) ->
            let (_, t_cond) = infer tenv cond_e in
            unify p t_bool t_cond;
            let (_, t_then) = infer tenv then_e in
            let (_, t_else) = infer tenv else_e in
            unify p t_then t_else;
            (tenv, t_then)
        | (p, Comp el) ->
            infer_list p tenv el
        | (_, Match _) ->
            (*TODO implement match*)
            (tenv, t_unit)
        | (_, Let (id, e)) ->
            let (_, t) = infer tenv e in
            let tenv = Env.extend id (ref t) tenv in
            (tenv, t_unit)
        | (_, LetRec (id, e)) ->
            let r = ref (new_tvar ()) in
            let tenv = Env.extend id r tenv in
            let (_, t) = infer tenv e in
            r := t;
            (tenv, t_unit)
        | (_, TypeDecl _) ->
            type_decl tenv x
        | (_, TypeDeclAnd el) ->
            (*TODO tenv *)
            List.iter (fun x -> ignore @@ type_decl tenv x) el;
            (tenv, t_unit)
        | (_, Module id) ->
            let tab = Symbol.set_module id in
            (tab.tenv, t_unit)
        | (_, Import (id, None)) ->
            Interp.import Eval.eval_top infer_top id;
            (tenv, t_unit)
        | (_, Import (id, Some aid)) ->
            Interp.import Eval.eval_top infer_top id;
            Symbol.rename_module id aid;
            (tenv, t_unit)
    in
    verbose @@ "infer: OUT: " ^ expr_to_string x ^ " => " ^ type_to_string ty;
    (tenv, ty)

and type_decl tenv e =
    match e with
    | (p, TypeDecl (id, tvl, t)) ->
        Symbol.insert_type id t;
        (tenv, t_unit)
    | _ -> failwith "type decl bug"

and infer_top e =
    let tab = Symbol.get_current_module () in
    let (tenv, t) = infer tab.tenv e in
    Symbol.set_current_tenv tenv;
    t

