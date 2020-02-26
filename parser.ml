
open Syntax

type t = {
    scanner : Scanner.t;
    mutable token : token_t;
}

let get_source_position pars =
    Scanner.get_source_position pars.scanner

let get_line pars =
    Scanner.get_line pars.scanner

let to_expr pars e = (get_line pars, e)

let debug_scope_flag = ref false
let debug_token_flag = ref false
let debug_indent = ref 0

let rec debug_show_space n =
    if n = 0 then ()
    else begin
        print_char ' ';
        debug_show_space (n-1)
    end
let debug_print_indent s =
    if !debug_scope_flag then begin
        debug_show_space !debug_indent;
        print_endline s
    end
let debug_parse_in s =
    if !debug_scope_flag then begin
        debug_show_space !debug_indent;
        incr debug_indent;
        print_endline ("IN " ^ s)
    end
let debug_parse_out s =
    if !debug_scope_flag then begin
        decr debug_indent;
        debug_show_space !debug_indent;
        print_endline ("OUT " ^ s)
    end

let debug_parse msg = debug_print_indent msg
let debug_token msg =
    if !debug_token_flag then
        print_endline msg


let error pars msg =
    raise (Error (get_source_position pars ^ ": " ^ msg))

let new_parser scan = { scanner = scan; token = Scanner.get_token scan }

let token_to_binop = function
    | PLUS -> BinAdd
    | MINUS -> BinSub
    | STAR -> BinMul
    | SLASH -> BinDiv
    | PERCENT -> BinMod
    | LT -> BinLT
    | LE -> BinLE
    | GT -> BinGT
    | GE -> BinGE
    | EQL -> BinEql
    | NEQ -> BinNeq
    | LOR -> BinLor
    | LAND -> BinLand
    | COLON -> BinCons
    | _ -> failwith "binop bug"

let token_to_unop = function
    | NOT -> UNot
    | MINUS -> UMinus
    | _ -> failwith "unop bug"

let is_unop = function
    | NOT | MINUS -> true | _ -> false

let is_mul_op = function
    | STAR | SLASH | PERCENT -> true | _ -> false

let is_add_op = function
    | PLUS | MINUS -> true | _ -> false

let is_equal_op = function
    | LT | LE | GT | GE | EQL | NEQ -> true | _ -> false

let is_logical_op = function
    | LOR | LAND -> true | _ -> false

let is_apply e t =
    match snd e with
    | Fn _ | Apply _ | Ident _ | IdentMod _ ->
        begin
            match t with
            | UNIT | NULL | ID _ | C_ID _ | BOOL_LIT _ | INT_LIT _
            | CHAR_LIT _ | FLOAT_LIT _ | STRING_LIT _ | LPAR | LSBRA -> true
            | _ -> false
        end
    | _ -> false


let peek_token pars = pars.token.token

let next_token pars =
    pars.token <- Scanner.get_token pars.scanner;
    debug_token @@ get_source_position pars ^ ": token = " ^ token_to_string @@ peek_token pars

let is_type_name pars =
    match peek_token pars with
    | C_ID _ | ID _ -> true
    | _ -> false

let is_type pars =
    match peek_token pars with
    | TVAR _ | C_ID _ | ID _ | LPAR -> true
    | _ -> false

let is_field_decl pars =
    match peek_token pars with
    | MUTABLE | ID _ -> true
    | _ -> false

let rec expect pars token =
    let current_token = peek_token pars in
    if current_token = token then
        next_token pars
    else if current_token = NEWLINE then
        (next_token pars; expect pars token)
    else
        error pars ("missing token '" ^ token_to_string token ^
            "' at '" ^ token_to_string current_token ^ "'")

let rec expect_id pars =
    match peek_token pars with
    | ID id -> next_token pars; id
    | NEWLINE -> next_token pars; expect_id pars
    | t -> error pars ("missing identifier at '" ^ token_to_string t ^ "'")

let rec expect_c_id pars =
    match peek_token pars with
    | C_ID id -> next_token pars; id
    | NEWLINE -> next_token pars; expect_c_id pars
    | t -> error pars ("missing capitalized identifier at '" ^ token_to_string t ^ "'")

let rec skip_newline pars =
    match peek_token pars with
    | NEWLINE ->
        next_token pars;
        skip_newline pars
    | _ -> ()

let rec expr_list_length = function
    | Null -> 0
    | Binary (BinCons, _, xs) -> 1 + expr_list_length (snd xs)
    | _ -> failwith "expr_list bug"

let rec parse_list_expr pars =
    debug_parse_in "parse_list_expr";
    let lhs = parse_expr pars in
    let rhs = 
        if peek_token pars = COMMA then begin
            next_token pars;
            skip_newline pars;
            parse_list_expr pars
        end else
            to_expr pars Null
    in
    debug_parse_out "parse_list_expr";
    to_expr pars (Binary (BinCons, lhs, rhs))

and parse_c_id_expr pars =
    debug_parse_in "parse_c_id_expr";
    let cid = expect_c_id pars in
    let e =
        if peek_token pars = DOT then begin
            next_token pars;
            skip_newline pars;
            match peek_token pars with
            | ID _ ->
                to_expr pars (IdentMod (cid, parse_id_expr pars))
            | C_ID _ ->
                to_expr pars (IdentMod (cid, parse_c_id_expr pars))
            | _ -> error pars "missing identifier"
        end else to_expr pars Unit
    in
    (* assign / construct / record *)
    debug_parse_out "parse_c_id_expr";
    e

and parse_id_expr pars =
    debug_parse_in "parse_id_expr";
    let lhs = to_expr pars (Ident (expect_id pars)) in
    let rec parse_rhs lhs =
        if peek_token pars = DOT then begin
            next_token pars;
            let id = expect_id pars in
            parse_rhs (to_expr pars (Record (lhs, id)))
        end else lhs
    in
    let e = parse_rhs lhs in
    (* assign *)
    debug_parse_out "parse_id_expr";
    e

and parse_simple pars =
    debug_parse_in "parse_simple";
    let res =
        match peek_token pars with
        | EOF ->
            to_expr pars Eof
        | UNIT ->
            next_token pars;
            to_expr pars Unit
        | NULL ->
            next_token pars;
            to_expr pars Null
        | C_ID _ ->
            parse_c_id_expr pars
        | ID _ ->
            parse_id_expr pars
        | BOOL_LIT b ->
            next_token pars;
            to_expr pars (BoolLit b)
        | INT_LIT i ->
            next_token pars;
            to_expr pars (IntLit i)
        | CHAR_LIT c ->
            next_token pars;
            to_expr pars (CharLit c)
        | FLOAT_LIT f ->
            next_token pars;
            to_expr pars (FloatLit f)
        | STRING_LIT s ->
            next_token pars;
            to_expr pars (StringLit s)
        | LPAR ->
            next_token pars;
            skip_newline pars;
            let e = parse_expr pars in
            let rec loop lst =
                let e = parse_expr pars in
                if peek_token pars = COMMA then begin
                    next_token pars;
                    skip_newline pars;
                    loop (e::lst)
                end else
                    List.rev (e::lst)
            in
            if peek_token pars = COMMA then begin
                next_token pars;
                skip_newline pars;
                let e2 = loop [] in
                expect pars RPAR;
                to_expr pars (Tuple (e::e2))
            end else begin
                expect pars RPAR;
                e
            end
        | LSBRA ->
            next_token pars;
            skip_newline pars;
            let e = parse_list_expr pars in
            expect pars RSBRA;
            if expr_list_length (snd e) = 0 then
                to_expr pars Null
            else
                e
        | t ->
            next_token pars;
            error pars ("syntax error at '" ^ token_to_string t ^ "'")
    in
    debug_parse_out "parse_simple";
    res

and parse_unary pars =
    debug_parse_in "parse_unary";
    let op = peek_token pars in
    let res =
        if is_unop op then begin
            next_token pars;
            let e = parse_simple pars in
            to_expr pars (Unary (token_to_unop op, e))
        end else
            parse_simple pars
    in
    debug_parse_out "parse_unary";
    res

and parse_apply pars =
    debug_parse_in "parse_apply";
    let rec parse_apply_rhs lhs =
        let a = parse_simple pars in
        let e = to_expr pars (Apply (lhs, a)) in
        if is_apply e (peek_token pars) then
            parse_apply_rhs e
        else
            e
    in
    let e = parse_unary pars in
    let res =
        if is_apply e (peek_token pars) then
            parse_apply_rhs e
        else
            e
    in
    debug_parse_out "parse_apply";
    res

and parse_mul pars =
    debug_parse_in "parse_mul";
    let rec parse_rhs lhs =
        let tt = peek_token pars in
        if not (is_mul_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_apply pars in
            parse_rhs (to_expr pars (Binary (op, lhs, rhs)))
        end
    in
    let e = parse_apply pars in
    let e = parse_rhs e in
    debug_parse_out "parse_mul";
    e

and parse_add pars =
    debug_parse_in "parse_add";
    let rec parse_rhs lhs =
        let tt = peek_token pars in
        if not (is_add_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_mul pars in
            parse_rhs (to_expr pars (Binary (op, lhs, rhs)))
        end
    in
    let e = parse_mul pars in
    let e = parse_rhs e in
    debug_parse_out "parse_add";
    e

and parse_cons pars =
    debug_parse_in "parse_cons";
    let rec parse_rhs lhs =
        let tt = peek_token pars in
        if tt <> COLON then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_add pars in
            to_expr pars (Binary (op, lhs, parse_rhs rhs))
        end
    in
    let e = parse_add pars in
    let e = parse_rhs e in
    debug_parse_out "parse_cons";
    e

and parse_equal pars =
    debug_parse_in "parse_equal";
    let lhs = parse_cons pars in
    let tt = peek_token pars in
    let e =
        if not (is_equal_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_cons pars in
            to_expr pars (Binary (op, lhs, rhs))
        end
    in
    debug_parse_out "parse_equal";
    e

and parse_logical pars =
    debug_parse_in "parse_logical";
    let rec parse_rhs lhs =
        let tt = peek_token pars in
        if not (is_logical_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_equal pars in
            parse_rhs (to_expr pars (Binary (op, lhs, rhs)))
        end
    in
    let e = parse_equal pars in
    let e = parse_rhs e in
    debug_parse_out "parse_logical";
    e

and parse_let pars =
    debug_parse_in "parse_let";
    next_token pars;
    skip_newline pars;
    let is_rec =
        if peek_token pars = REC then
            (next_token pars; true)
        else
            false
    in
    let id = expect_id pars in
    expect pars EQ;
    skip_newline pars;
    let e = parse_expr pars in
    skip_newline pars;
    let e =
        if is_rec then
            to_expr pars (LetRec (id, e))
        else
            to_expr pars (Let (id, e))
    in
    debug_parse_out "parse_let";
    e

and parse_fun pars =
    debug_parse_in "parse_fun";
    next_token pars;
    skip_newline pars;
    let id = expect_id pars in
    let args = parse_params pars in
    expect pars EQ;
    skip_newline pars;
    let e = List.fold_right (fun arg body -> to_expr pars (Fn (arg, body))) args (parse_expr pars)
    in
    debug_parse_out "parse_fun";
    to_expr pars (LetRec (id, e))

and parse_param_list pars args =
    debug_parse_in "parse_param_list";
    let e =
        match peek_token pars with
        | WILDCARD ->
            next_token pars;
            parse_param_list pars (to_expr pars WildCard :: args)
        | ID id ->
            next_token pars;
            parse_param_list pars (to_expr pars (Ident id) :: args)
        | _ ->
            List.rev args
    in
    debug_parse_out "parse_param_list";
    e

and parse_params pars =
    debug_parse_in "parse_params pars";
    let e =
        if peek_token pars = UNIT then
            (next_token pars; [to_expr pars Unit])
        else
            parse_param_list pars []
    in
    debug_parse_out "parse_params pars";
    e

and parse_fn pars =
    debug_parse_in "parse_fn";
    next_token pars;
    skip_newline pars;
    let args = parse_params pars in
    expect pars RARROW;
    skip_newline pars;
    let e = List.fold_right (fun arg body -> to_expr pars ( Fn (arg, body))) args (parse_expr pars) in
    debug_parse_out "parse_fn";
    e

and parse_if pars =
    debug_parse_in "parse_if";
    next_token pars;
    skip_newline pars;
    let e1 = parse_expr pars in
    expect pars THEN;
    skip_newline pars;
    let e2 = parse_expr pars in
    skip_newline pars;
    let e3 =
        if peek_token pars = ELSE then begin
            next_token pars;
            skip_newline pars;
            parse_expr pars
        end else to_expr pars Unit
    in
    debug_parse_out "parse_if";
    to_expr pars (If (e1, e2, e3))

and parse_match pars =
    next_token pars;
    to_expr pars Unit    (*TODO*)

and parse_expr_list pars =
    debug_parse_in "parse_expr_list";
    let rec loop () =
        match peek_token pars with
        | EOF | RBRACE -> []
        | SEMI ->
            next_token pars;
            if peek_token pars = RBRACE then []
            else loop ()
        | _ ->
            let e = parse_expr pars in
            skip_newline pars;
            e::(loop ())
    in
    let e = loop () in
    debug_parse_in "parse_expr_list";
    e

and parse_compound pars =
    debug_parse_in "parse_compound";
    next_token pars;
    skip_newline pars;
    let e = parse_expr_list pars in
    expect pars RBRACE;
    skip_newline pars;
    debug_parse_out "parse_compound";
    to_expr pars (Comp e)

and parse_expr pars =
    debug_parse_in "parse_expr";
    let e =
        match peek_token pars with
        | EOF -> to_expr pars Eof
        | NEWLINE | SEMI -> next_token pars; parse_expr pars
        | LET -> parse_let pars
        | FUN -> parse_fun pars
        | FN -> parse_fn pars
        | IF -> parse_if pars
        | MATCH -> parse_match pars
        | LBRACE -> parse_compound pars
        | _ -> parse_logical pars
    in
    debug_parse_out "parse_expr";
    e

let parse_type_name pars cid_opt =
    debug_parse_in "parse_type_name";
    let rec loop name_lst cid_opt =
        match cid_opt with
        | Some cid ->
            loop (cid::name_lst) None
        | _ ->
            (match peek_token pars with
            | C_ID cid ->
                next_token pars;
                loop (cid::name_lst) None
            | _ ->
                let id = expect_id pars in
                (List.rev name_lst, id))
    in
    let res = loop [] cid_opt in
    debug_parse_out "parse_type_name";
    res

let rec parse_constr_type pars cid_opt =
    debug_parse_in "parse_constr_type";
    let res =
        let t = match cid_opt with
            | Some _ ->
                TConstr (parse_type_name pars cid_opt, None)
            | _ ->
                (match peek_token pars with
                | TVAR n ->
                    next_token pars;
                    TVar (n, ref None)
                | C_ID _ | ID _ ->
                    TConstr (parse_type_name pars None, None)
                | LPAR ->
                    next_token pars;
                    let t = parse_type pars None in
                    if peek_token pars <> COMMA then begin
                        expect pars RPAR;
                        t
                    end else begin
                        let rec loop lst =
                            let t = parse_type pars None in
                            if peek_token pars = COMMA then begin
                                next_token pars;
                                skip_newline pars;
                                loop (t::lst)
                            end else
                                List.rev (t::lst)
                        in
                        let tl = loop [t] in
                        expect pars RPAR;
                        TTuple tl
                    end
                | _ -> failwith "parse_constr_type bug")
        in
        if is_type_name pars then
            TConstr (parse_type_name pars None, Some t)
        else
            t
    in
    debug_parse_out "parse_constr_type";
    res

and parse_tuple_type pars cid_opt =
    debug_parse_in "parse_tuple_type";
    let t = parse_constr_type pars cid_opt in
    let rec loop res =
        if peek_token pars = STAR then
            (next_token pars; loop ((parse_constr_type pars None) :: res))
        else List.rev res
    in
    let res =
        if peek_token pars = STAR then
            TTuple (loop [t])
        else t
    in
    debug_parse_out "parse_tuple_type";
    res

and parse_type pars cid_opt =
    debug_parse_in "parse_type";
    let t = parse_tuple_type pars cid_opt in
    let res =
        if peek_token pars = RARROW then
            (next_token pars; TFun (t, parse_type pars None))
        else t
    in
    debug_parse_out "parse_type";
    res

let parse_variant_elem pars cid_opt =
    debug_parse_in "parse_variant_elem";
    let res =
        match cid_opt with
        | None ->
            begin
                match peek_token pars with
                | C_ID cid ->
                    begin
                        next_token pars;
                        if is_type pars then
                            (cid, Some (parse_type pars None))
                        else
                            (cid, None)
                    end
                | t -> error pars ("expect Capitalized ID at '" ^ token_to_string t ^ "'")
            end
        | Some cid ->
            if is_type pars then
                (cid, Some (parse_type pars None))
            else
                (cid, None)
    in
    debug_parse_out "parse_variant_elem";
    res

let parse_variant_decl pars cid_opt =
    debug_parse_in "parse_variant_decl";
    if peek_token pars = OR then
        next_token pars;
    let rec loop res =
        if peek_token pars = OR then begin
            next_token pars;
            let (id, t) = parse_variant_elem pars None in
            loop ((id, t)::res)
        end else
            List.rev res
    in
    let (id, t) = parse_variant_elem pars cid_opt in
    let res = TVariantDecl (loop ([(id, t)])) in
    debug_parse_out "parse_variant_decl";
    res

let parse_field_decl pars =
    debug_parse_in "parse_field_decl";
    let mut_flag =
        if peek_token pars = MUTABLE then
            (next_token pars; Mutable)
        else
            Immutable
    in
    let id = expect_id pars in
    expect pars DCOLON;
    let t = parse_type pars None in
    let res = (id, t, mut_flag) in
    debug_parse_out "parse_field_decl";
    res

let parse_record_decl pars =
    debug_parse_in "parse_record_decl";
    next_token pars;
    let rec loop fl =
        if is_field_decl pars then
            let f = parse_field_decl pars in
            if peek_token pars <> SEMI then
                List.rev (f::fl)
            else begin
                next_token pars;
                loop (f::fl)
            end
        else
            List.rev fl
    in
    let res = TRecordDecl (loop []) in
    debug_parse_out "parse_record_decl";
    res

let parse_type_params_opt pars =
    debug_parse_in "parse_type_params_opt";
    let rec parse_type_param_list res =
        match peek_token pars with
        | TVAR n ->
            next_token pars;
            if peek_token pars = COMMA then
                (next_token pars; parse_type_param_list (n :: res))
            else List.rev (n :: res)
        | RPAR -> List.rev res
        | t -> error pars ("expect type variable at '" ^ token_to_string t ^ "'")
    in
    let res = 
        match peek_token pars with
        | TVAR n ->
            next_token pars;
            [n]
        | LPAR ->
            next_token pars;
            let lst = parse_type_param_list [] in
            expect pars RPAR;
            lst
        | _ -> []
    in
    debug_parse_out "parse_type_params_opt";
    res

let parse_type_def pars =
    debug_parse_in "parse_type_def";
    let tvl = parse_type_params_opt pars in
    let id = expect_id pars in
    expect pars EQ;
    let td = match peek_token pars with
        | LBRACE -> parse_record_decl pars
        | OR -> parse_variant_decl pars None
        | C_ID cid ->
            next_token pars;
            if peek_token pars = DOT then
                (next_token pars; TTypeDecl (parse_type pars (Some cid)))
            else
                parse_variant_decl pars (Some cid)
        | _ -> TTypeDecl (parse_type pars None)
    in
    let e = to_expr pars (TypeDecl (id, tvl, td)) in
    debug_parse_out "parse_type_def";
    e

let parse_type_decl pars =
    debug_parse_in "parse_type_decl";
    next_token pars;
    let rec loop e =
        if peek_token pars <> AND then
            e
        else begin
            next_token pars;
            loop @@ to_expr pars (TypeDeclAnd (e, parse_type_def pars))
        end
    in
    let e = loop (parse_type_def pars) in
    debug_parse_out "parse_type_decl";
    e

let parse_module pars =
    debug_parse_in "parse_module";
    next_token pars;
    let id = expect_c_id pars in
    debug_parse_out "parse_module";
    to_expr pars (Module id)

let parse_import pars =
    debug_parse_in "parse_import";
    next_token pars;
    let id = expect_c_id pars in
    let rename =
        if peek_token pars = AS then begin
            next_token pars;
            Some (expect_c_id pars)
        end else
            None
    in
    debug_parse_out "parse_import";
    to_expr pars (Import (id, rename))

let rec parse_decl pars =
    debug_parse_in "parse_decl";
    let e =
        match peek_token pars with
        | EOF -> to_expr pars Eof
        | NEWLINE | SEMI -> next_token pars; parse_decl pars
        | MODULE -> parse_module pars
        | IMPORT -> parse_import pars
        | TYPE -> parse_type_decl pars
        | _ -> parse_expr pars
    in
    debug_parse_out "parse_decl";
    e

let parse scanner =
    let pars = new_parser scanner in
    parse_decl pars
    
