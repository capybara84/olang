
open Syntax

let warning p msg = print_endline @@ get_position_string p ^ ": Warning: " ^ msg

let default_directory = "./"
let default_extension = ".wt"

let make_module_filename name =
    default_directory ^ String.uncapitalize name ^ default_extension


let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let load_source eval_fn infer_fn filename =
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
                let t = infer_fn e in
                if !g_verbose then
                    print_endline @@ expr_to_string e ^ " : " ^ type_to_string t;
                let v = eval_fn e in
                if !g_verbose then
                    print_endline @@ value_to_string v ^ " : " ^ type_to_string t;
                loop ()
        in loop ()
    with Error s | Sys_error s -> print_endline s

let import eval_fn infer_fn id =
    if Symbol.exist_module id then
        ()
    else begin
        let filename = make_module_filename id in
        let prev = Symbol.get_current_module () in
        ignore (Symbol.set_module id);
        (try
            load_source eval_fn infer_fn filename
        with Error s | Sys_error s -> print_endline s);
        Symbol.set_current_module prev
    end


let rec top_level eval_fn infer_fn =
    try
        print_string "> ";
        flush stdout;
        let scan = Scanner.from_string "STDIN" @@ input_line stdin in
        let rec loop () =
            let e = Parser.parse_one scan in
            if snd e = Eof then
                () 
            else begin
                if !g_verbose then
                    print_endline @@ expr_to_string e;
                if !g_output_source then
                    print_endline @@ expr_to_string_src e;
                let t = infer_fn e in
                let v = eval_fn e in
                print_endline @@ value_to_string v ^ " : " ^ type_to_string t;
                loop ()
            end
        in
        loop ();
        top_level eval_fn infer_fn
    with
        | Error s -> print_endline s; top_level eval_fn infer_fn
        | Sys_error s -> print_endline s
        | End_of_file -> ()


