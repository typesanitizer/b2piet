open Batteries
open Cmdliner

(* Optimisations *)
let loops_off =
  let doc = "Make drawings based using the fact that there are no loops." in
  Arg.(value & flag & info ["no-loops"] ~doc)
let cancel_off =
  let doc = "Turn off cancellations \\(fusion of \\(+,-\\), \\(>,<\\), \
             deleting \\(],[\\)\\). Also disables related warnings." in
  Arg.(value & flag & info ["c"; "no-cancel"] ~doc)
let condense_off =
  let doc = "Turn off condensation; Piet will perform additions and \
             subtractions one at a time. Primarily for debugging." in
  Arg.(value & flag & info ["dgb-no-condense"] ~doc)
let fast_off =
  let doc = "Turn off fast pushes; Piet will push numbers suboptimally onto \
             the stack. The resulting output will be easier to understand \
             at the expense of (very likely) a larger image size. \
             For example, if this flag is set, the number 8 will be pushed \
             using 8 codels (ignoring the codel required to actually push as \
             it will be reused later). If this option is not enabled, the \
             same push can be done using 6 (+1) codels: pushing 2 (2 codels), \
             duplicating twice (2 codels) and multiplying twice (2 codels)." in
  Arg.(value & flag & info ["f"; "no-fast-push"] ~doc)
let shrink_rolls =
  let doc = "[EXPERIMENTAL] Try to reduce size of roll operations for \
             Piet output. Output may be incorrect." in
  Arg.(value & flag & info ["S"; "shrink-rolls"] ~doc)

(* Soundness *)
let stack_auto =
  let doc = "The size of the Piet stack is 'automatically' determined by \
             running the Brainfuck code and checking for the maximum value \
             of the data pointer over the program's life." in
  Arg.(value & flag & info ["stack-auto"] ~doc)
let stack_size =
  let doc = "The size of the Piet stack for operation. For correctness \
             in programs with loops, this must be set to a value greater than \
             the value of the maximum cell number (starting at zero) reached \
             by the Brainfuck input. This size is ignored if the no-loops \
             argument is given. $(b,Note): depending on the program, the value \
             of this argument may need to be adjusted significantly. While toy \
             programs may work even with stack=5, we have seen \"serious\" \
             programs require up to stack=150. If your program isn't \
             translating correctly, try increasing this value.\n\
             This argument is ignored if stack-auto is also supplied." in
  Arg.(value & opt int 8 & info ["s"; "stack"] ~doc)
let no_json =
  let doc = "$(i,Do not) save or reuse metadata stored in a local json file. \
             For example, if you run the program with stack-auto twice on a \
             file, the appropriate stack size will be saved so that it need \
             not be recomputed again the second time. Similarly, the \
             computation for fast pushes will also be saved/reused unless \
             no-fast-push is explicitly given. Using this flag turns off \
             this save/reuse functionality." in
  Arg.(value & flag & info ["no-json"] ~doc)

(* Debugging *)
let ast_on =
  let doc = "Print the Piet IR tree generated after translation. This will \
             generally $(i,not) represent the final program exactly unless the \
             flag for turning off fast pushes is also supplied. \
             Primarily for debugging." in
  Arg.(value & flag & info ["I"; "ir-tree"] ~doc)

(* Input and output *)
let codel_dim =
  let doc = "The width of codels in pixels." in
  Arg.(value & opt int 8 & info ["d"; "dim"] ~doc)
let output_fname =
  let doc = "The name of the image file to be saved. Supported file formats \
             include `.png`, `.ppm` and `.bmp`." in
  Arg.(value & opt string "a.png" & info ["o"; "output"] ~doc)
let input_fname =
  let doc = "Name of input file, if any. Otherwise, a quoted string is \
             required." in
  Arg.(value & opt string "" & info ["i"; "input"] ~doc)
let rewrite_file =
  let doc = "Whether input file should be rewritten to remove warnings." in
  Arg.(value & flag & info ["rewrite"] ~doc)
let input_str =
  let doc = "The Brainfuck code to be transpiled." in
  Arg.(value & pos 0 string "+" & info [] ~docv:"STRING" ~doc)

let info =
  let doc = "Convert Brainfuck programs into Piet programs." in
  let man = [
    `S   Manpage.s_examples;
    `P   "Basic usage:";
    `Pre "    \\$ ./b2piet.byte \"+++++++++++++++++++++++++++++++++.\"";
    `P   "saves a Piet program to `a.png` that prints '!'.";
    `P   "We can also use code from a `.b` file:";
    `Pre "    \\$ ./b2piet.byte --input samples/hello.b --output hello.png";
    `P   "The default stack size of 8 is sufficient for small programs but \
          will give incorrect results for non-trivial ones. One should try out \
          different combinations manually. In the following case, we have \
          checked that a stack size of 18 is sufficient.";
    `Pre "    \\$ ./b2piet.byte --stack=18 --input samples/homura.b";
    `S Manpage.s_bugs;
    `P ("Please open an issue on https://github.com/theindigamer/b2piet/ for \
         any bugs, or if you need help with using this software. If you do \
         not have a Github account and do not wish to create one, please send \
         me an email (see below) with the bug report.");
    `S Manpage.s_authors;
    `P  "Varun Gandhi <theindigamer15 AT gmail DOT com>"
  ]
  in Term.info "b2piet" ~doc ~exits:Term.default_exits ~man

let get_stack_size ?(use_json = true) ?(stack_auto = true)
    ?(stack_size = 8) ~str ~bfinstr meta =
  let default = (Utils.MetaJson.empty, stack_size, None) in
  if use_json then
    let m = BatOption.get meta in
    let calc_ssize hash =
      let (max_dp, err) = Translator.interpret bfinstr in
      (* TODO: Add error handling *)
      let _ = print_endline @@ Printf.sprintf
          "Minimum stack size: %d" max_dp in
      (Utils.MetaJson.set_ssize hash max_dp m, max_dp, err) in
    match Utils.MetaJson.get_ssize str m with
    | hash, Some z -> (m, z, None)
    | hash, None -> if stack_auto then calc_ssize hash else default
  else if stack_auto then
    Translator.interpret bfinstr
    |> fun (ssize, err) -> (Utils.MetaJson.empty, ssize, err)
  else default

let main
    loops_off cancel_off condense_off fast_off shrink_rolls
    stack_auto stack_size no_json
    ast_on
    codel_dim output_fname input_fname rewrite_file input_str
  =

  let loops = not loops_off in
  let cancel = not cancel_off in
  let condense = not condense_off in
  let fast = not fast_off in
  let use_json = not no_json in
  let str =
    if input_fname <> "" then
      File.lines_of input_fname
      |> List.of_enum
      |> String.concat "\n"
    else
      let file_flag_warning =
        print_endline %
        Printf.sprintf "Warning: The input string contains a %c character.\n\
                        If you intended to use a file as input, use the `-i` \
                        flag. \n\
                        See `bf2piet.xyz --help` for more information."
      in
      if String.contains input_str '/' then file_flag_warning '/'
      else if String.contains input_str '\\' then file_flag_warning '\\';
      input_str in

  let meta = if use_json then Some Utils.MetaJson.get else None in

  match Parser.parse ~loops_off str with
  | (bfinstr_l, []) ->
    let bfinstr_lopt =
      if cancel then
        let (bfinstr_l, warn_l) = Optimiser.optimise bfinstr_l in
        List.iter (print_string % Optimiser.warn_msg) warn_l;
        bfinstr_l
      else bfinstr_l in

    let (meta, stack_size, err) =
      get_stack_size ~use_json ~stack_auto ~stack_size ~str
        ~bfinstr:bfinstr_lopt meta in
    (* TODO: Add error handling *)
    let (piet_ir, err) =
      Translator.translate bfinstr_lopt
        ~condense ~loops_present:loops ~stack_size ~shrink_rolls in
    if ast_on then Utils.PietIR.print_ast piet_ir;
    (* let _ = Painter.interpret piet_ir in *)
    (* exit 0; *)
    let (meta, fpl) =
      Utils.MetaJson.get_fast_push_table
        ~use_json ~num_ops:5 ~stack_size meta in
    (if use_json then Utils.MetaJson.save meta);
    print_endline @@ Painter.domain_show fpl piet_ir;
    (* let pic = Painter.(if fast then paint (Fast fpl) Linear *)
    (*                    else paint Literal Linear) piet_ir in *)
    (* Printer.save_picture output_fname codel_dim pic; *)

  | (_, err_l) ->
    List.iter (Parser.error_msg %> print_string) err_l

let main_t =
  Term.(const main
        $ loops_off $ cancel_off $ condense_off $ fast_off $ shrink_rolls
        $ stack_auto $ stack_size $ no_json
        $ ast_on
        $ codel_dim $ output_fname $ input_fname $ rewrite_file $ input_str)

let _ = Term.exit @@ Term.eval (main_t, info)
