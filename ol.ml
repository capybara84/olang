open Syntax

let rec top_level env =
    try
        print_string "> ";
        flush stdout;
        let scan = Scanner.from_string "STDIN" @@ input_line stdin in
        let rec loop env =
            let e = Parser.parse scan in
            if snd e = Eof then
                env 
            else begin
                if !g_verbose then
                    print_endline @@ expr_to_string e;
                let (env, v) = Eval.eval_decl env e in
                print_endline @@ value_to_string v;
                loop env
            end
        in
        let env = loop env in
        top_level env 
    with
        | Error s -> (print_endline s; top_level env)
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let load_source env filename =
    try
        let text = load_file filename in
        let scan = Scanner.from_string filename text in
        let rec loop env =
            let e = Parser.parse scan in
            match e with
            | (_, Eof) -> env
            | _ ->
                let (new_env, v) = Eval.eval_decl env e in
                if v <> VUnit then
                    print_endline "Warning: The expression should have type unit";
                loop new_env
        in loop env
    with
        | Error s | Sys_error s -> print_endline s; env
        | End_of_file -> env

let main () =
    let filenames = ref [] in
    let do_test = ref false in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> g_verbose := true), " verbose");
            ("-t", Arg.Unit (fun () -> do_test := true),   " test");
            ("-dps", Arg.Unit (fun () -> Parser.debug_scope_flag := true), " parser scope debug");
            ("-dpt", Arg.Unit (fun () -> Parser.debug_token_flag := true), " parser token debug");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: ol [-v][-t] filename...";
    let env = ref (Builtins.init ()) in
    List.iter (fun name -> env := load_source !env name) (List.rev !filenames);
    if !do_test then
        Test.test !env
    else if List.length !filenames = 0 then
        top_level !env

let () =
    main ()
