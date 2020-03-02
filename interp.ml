
open Syntax

let warning p msg = print_endline @@ get_position_string p ^ ": Warning: " ^ msg


let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let load_source eval_fn filename =
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
                let v = eval_fn e in
                if v <> VUnit then
                    warning (fst e) "The expression should have type unit";
                loop ()
        in loop ()
    with Error s | Sys_error s -> print_endline s

let rec top_level eval_fn =
    try
        print_string "> ";
        flush stdout;
        let scan = Scanner.from_string "STDIN" @@ input_line stdin in
        let rec loop env =
            let e = Parser.parse_one scan in
            if snd e = Eof then
                () 
            else begin
                if !g_verbose then
                    print_endline @@ expr_to_string e;
                if !g_output_source then
                    print_endline @@ expr_to_string_src e;
                let v = eval_fn e in
                print_endline @@ value_to_string v;
                loop ()
            end
        in
        loop ();
        top_level eval_fn
    with
        | Error s -> print_endline s; top_level eval_fn
        | Sys_error s -> print_endline s
        | End_of_file -> ()


