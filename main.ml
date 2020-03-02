open Syntax


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
    List.iter (Interp.load_source Eval.eval_top Type.infer_top) (List.rev !filenames);
    if !do_test then
        Test.test ()
    else if List.length !filenames = 0 then
        Interp.top_level Eval.eval_top Type.infer_top

