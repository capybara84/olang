open Syntax

let rec top_level () =
    try
        print_string "> ";
        flush stdout;
        let scan = Scanner.from_string "STDIN" @@ input_line stdin in
        let rec loop () =
            let e = Parser.parse scan in
            if snd e <> Eof then begin
                print_endline @@ expr_to_string e;
                loop ()
            end
        in
        loop ();
        top_level ()
    with
        | Error s -> (print_endline s; top_level ())
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
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: ol [-v][-t] filename...";
    if !do_test then
        Test.test()
    else if List.length !filenames = 0 then
        top_level ()

let () =
    main ()
