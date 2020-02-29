open Syntax

let rec top_level () =
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
                let v = Eval.eval_top e in
                print_endline @@ value_to_string v;
                loop ()
            end
        in
        loop ();
        top_level () 
    with
        | Error s -> print_endline s; top_level ()
        | Sys_error s -> print_endline s
        | End_of_file -> ()


let main () =
    let filenames = ref [] in
    let do_test = ref false in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> g_verbose := true), " verbose");
            ("-t", Arg.Unit (fun () -> do_test := true),   " test");
            ("-dps", Arg.Unit (fun () -> Parser.debug_scope_flag := true), " parser scope debug");
            ("-dpt", Arg.Unit (fun () -> Parser.debug_token_flag := true), " parser token debug");
            ("-os", Arg.Unit (fun () -> g_output_source := true), " output source style");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v][-t] filename...";
    Symbol.set_default_module ();
    Builtins.init ();
    List.iter Eval.load_source (List.rev !filenames);
    if !do_test then
        Test.test ()
    else if List.length !filenames = 0 then
        top_level ()

