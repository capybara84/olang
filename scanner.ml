
open Syntax

type t = {
    filename : string;
    source : string;
    len : int;
    mutable current : int;
    mutable cur_col : int;
    mutable line : int;
    mutable col : int;
}

let get_pos scan = { filename = scan.filename; line = scan.line; col = scan.col }

let get_source_position_string scan =
    get_position_string @@ get_pos scan

let error scan msg =
    raise (Error (get_source_position_string scan ^ ": " ^ msg))

let is_end scan =
    scan.current = scan.len

let peek scan =
    if is_end scan then
        None
    else
        Some scan.source.[scan.current]

let next_char scan =
    if is_end scan then
        ()
    else begin
        scan.current <- scan.current + 1;
        scan.cur_col <- scan.cur_col + 1
    end

let next_line scan =
    next_char scan;
    scan.line <- scan.line + 1;
    scan.cur_col <- 1;
    scan.col <- 1

let cut_token pred scan =
    let buffer = Buffer.create 5 in
    let rec loop () =
        match peek scan with
        | Some ch when pred ch ->
            begin
                Buffer.add_char buffer ch;
                next_char scan;
                loop ()
            end
        | _ -> ()
    in loop ();
    Buffer.contents buffer

let scan_number scan =
    (*TODO scan float number *)
    let is_digit = function '0'..'9' -> true | _ -> false in
    INT_LIT (int_of_string (cut_token is_digit scan)) 

let is_ident = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '#' -> true | _ -> false

let scan_c_ident scan =
    let id = cut_token is_ident scan in
    C_ID id

let scan_ident scan =
    let id = cut_token is_ident scan in
    match id with
    | "_" -> WILDCARD | "module" -> MODULE | "import" -> IMPORT | "as" -> AS
    | "decl" -> DECL | "type" -> TYPE | "and" -> AND | "let" -> LET | "rec" -> REC
    | "fun" -> FUN | "fn" -> FN | "if" -> IF | "then" -> THEN | "else" -> ELSE
    | "match" -> MATCH | "when" -> WHEN | "mutable" -> MUTABLE
    | _ -> ID id

let get_char scan =
    match peek scan with
    | Some '\\' ->
        begin
            next_char scan;
            (*TODO parse \000 style code *)
            (*TODO skip \ at end of line *)
            match peek scan with
            | Some 'n' -> '\n'
            | Some 'r' -> '\r'
            | Some 't' -> '\t'
            | Some ch -> ch
            | None -> error scan "unexpected EOF";
        end
    | Some ch -> ch
    | None -> error scan "unexpected EoF"

let scan_char scan =
    let is_alpha = function 'a'..'z' -> true | _ -> false in
    let char_to_int c = Char.code c - Char.code 'a' in
    next_char scan;
    let c = get_char scan in
    next_char scan;
    if is_alpha c then
        if peek scan = Some '\'' then
            (next_char scan; CHAR_LIT c)
        else
            TVAR (char_to_int c)
    else
        match peek scan with
        | Some '\'' ->
            next_char scan;
            CHAR_LIT c
        | Some _ -> error scan "missing single-quote"
        | None -> error scan "Unexpected EOF"

let scan_string scan =
    next_char scan;
    let buffer = Buffer.create 10 in
    let rec loop () =
        match peek scan with
        | Some '"' ->
            next_char scan
        | None ->
            error scan "unexpected eof"
        | _ ->
            let c = get_char scan in
            Buffer.add_char buffer c;
            next_char scan;
            loop ()
    in
    loop ();
    STRING_LIT (Buffer.contents buffer)

let rec skip_newline scan =
    next_line scan;
    match peek scan with
    | Some '\n' -> skip_newline scan
    | _ -> NEWLINE

let rec skip_comment scan =
    next_char scan;
    match peek scan with
    | Some '\n' -> skip_newline scan
    | _ -> skip_comment scan

let rec skip_nested_comment scan =
    next_char scan;
    let rec loop () =
        match peek scan with
        | Some '*' ->
            next_char scan;
            if peek scan = Some '/' then
                next_char scan
            else
                loop ()
        | Some '/' ->
            next_char scan;
            if peek scan = Some '*' then
                skip_nested_comment scan
            else ();
            loop ()
        | Some '\n' ->
            next_line scan;
            loop ()
        | _ ->
            next_char scan;
            loop ()
    in loop ()

let rec scan_token scan =
    let scan_token2 ch token_type2 token_type1 =
        next_char scan;
        match peek scan with
        | Some c when c = ch ->
            (next_char scan; token_type2)
        | _ -> token_type1
    in
    scan.col <- scan.cur_col;
    match peek scan with
    | None -> EOF
    | Some ' ' | Some '\t' | Some '\r' -> next_char scan; scan_token scan
    | Some '\n' -> skip_newline scan
    | Some '0'..'9' -> scan_number scan
    | Some 'a'..'z' | Some '_' | Some '#' -> scan_ident scan
    | Some 'A'..'Z' -> scan_c_ident scan
    | Some '\'' -> scan_char scan
    | Some '"' -> scan_string scan
    | Some '+' -> next_char scan; PLUS
    | Some '*' -> next_char scan; STAR
    | Some '%' -> next_char scan; PERCENT
    | Some ',' -> next_char scan; COMMA
    | Some ';' -> next_char scan; SEMI
    | Some '.' -> scan_token2 '.' RANGE DOT
    | Some '-' -> scan_token2 '>' RARROW MINUS
    | Some '=' -> scan_token2 '=' EQL EQ
    | Some '!' -> scan_token2 '=' NEQ NOT
    | Some '>' -> scan_token2 '=' GE GT
    | Some '|' -> scan_token2 '|' LOR OR
    | Some '&' -> scan_token2 '&' LAND AMP
    | Some '[' -> scan_token2 ']' NULL LSBRA
    | Some ']' -> next_char scan; RSBRA
    | Some ')' -> next_char scan; RPAR
    | Some '(' -> scan_token2 ')' UNIT LPAR
    | Some '{' -> next_char scan; LBRACE
    | Some '}' -> next_char scan; RBRACE
    | Some ':' -> scan_token2 '=' ASSIGN COLON
    | Some '<' ->
        begin
            next_char scan;
            match peek scan with
            | Some '=' ->
                next_char scan;
                LE
            | Some '-' ->
                next_char scan;
                LARROW
            | _ -> LT
        end
    | Some '/' ->
        begin
            next_char scan;
            match peek scan with
            | Some '*' ->
                skip_nested_comment scan; 
                scan_token scan
            | Some '/' ->
                next_char scan;
                skip_comment scan
            | _ ->
                SLASH
        end
    | Some c ->
        begin
            next_char scan;
            error scan ("illegal character '" ^ String.make 1 c ^ "'")
        end

let get_token scan =
    let tok = scan_token scan in
    let pos = { filename = scan.filename; line = scan.line; col = scan.col } in
    { token = tok; pos = pos; }

let get_tokens scan =
    let rec loop result =
        let t = get_token scan in
        match t.token with
        | EOF -> t::result
        | _ -> loop (t::result)
    in 
    List.rev (loop [])

let from_string name s =
    { filename = name; source = s; len = String.length s; current = 0; cur_col = 1; line = 1; col = 1 }

