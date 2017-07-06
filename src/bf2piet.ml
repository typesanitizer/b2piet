open Batteries
open Cmdliner

(* Optimisations *)
let loops_off =
  let doc = "Make drawings based using the fact that there are no loops." in
  Arg.(value & flag & info ["no-loops"] ~doc)
let cancel_off =
  let doc = "Turn off cancellations \\(fusion of \\(+,-\\), \\(>,<\\), \
             deleting \\(][\\)\\). Also disables related warnings." in
  Arg.(value & flag & info ["c"; "no-cancel"] ~doc)
let condense_off =
  let doc = "Turn off condensation; Piet will perform additions and \
             subtractions one at a time." in
  Arg.(value & flag & info ["C"; "no-condense"] ~doc)
let shrink_rolls =
  let doc = "[EXPERIMENTAL] Try to reduce size of roll operations for \
             Piet output. Output may be incorrect." in
  Arg.(value & flag & info ["S"; "shrink"] ~doc)

(* Soundness *)
let stack_size =
  let doc = "The size of the Piet stack for operation. For correctness \
             in programs with loops, this must be set to a value greater than \
             the value of the maximum cell number (starting at zero) reached \
             by the Brainfuck input. This size is ignored if the no-loops \
             argument is given. $(b,Note): depending on the program, the value \
             of this argument may need to be adjusted significantly. If your \
             program isn't translating correctly, try changing this value." in
  Arg.(value & opt int 8 & info ["s"; "stack"] ~doc)

(* Debugging *)
let ast_on =
  let doc = "Print the Piet IR tree generated after translation." in
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
             required. If both options are given, the string is ignored." in
  Arg.(value & opt string "" & info ["i"; "input"] ~doc)
let input_str =
  let doc = "The Brainfuck code to be transpiled." in
  Arg.(value & pos 0 string "+" & info [] ~docv:"STRING" ~doc)

let info =
  let doc = "Convert Brainfuck programs into Piet programs." in
  let man = [
    `S   Manpage.s_examples;
    `P   "Basic usage:";
    `Pre "    \\$ ./bf2piet.byte \"+++++++++++++++++++++++++++++++++.\"";
    `P   "saves a Piet program to `a.png` that prints '!'.";
    `P   "We can also use code from a `.bf` file:";
    `Pre "    \\$ ./bf2piet.byte --input bf_samples/hello.bf --output hello.png";
    `P   "The default stack size of 8 is sufficient for small programs but \
          will give incorrect results for non-trivial ones. One should try out \
          different combinations manually. In the following case, we have \
          checked that a stack size of 18 is sufficient.";
    `Pre "    \\$ ./bf2piet.byte --stack=18 --input bf_samples/homura.bf";
    `S Manpage.s_bugs;
    `P ("Please open an issue on https://github.com/theindigamer/bf-piet/ for \
         any bugs, or if you need help with using this software. If you do \
         not have a Github account and do not wish to create one, please send \
         an email to `theindigamer15 AT gmail DOT com` with the bug report.");
    `S Manpage.s_authors;
    `P  "theindigamer"
  ]
  in Term.info "bf2piet" ~doc ~exits:Term.default_exits ~man

let main
    loops_off cancel_off condense_off shrink_rolls
    stack_size
    ast_on
    codel_dim output_fname input_fname input_str
  =

  let cancel = not cancel_off in
  let condense = not condense_off in
  let loops = not loops_off in
  let str =
    if input_fname <> "" then
      BatEnum.reduce (^) @@ BatFile.lines_of input_fname
    else input_str in

  match Parser.parse str loops_off with
  | (bfip_l, []) ->
    let bfip_lopt =
      if cancel then
        let (bfip, warn_l) = Optimiser.optimise bfip_l in
        List.iter (print_string % Optimiser.warn_msg) warn_l;
        bfip
      else bfip_l in
    let (piet_ir, err) =
      Translator.translate bfip_lopt condense loops stack_size shrink_rolls in
    if ast_on then Utils.PietIR.print_ast piet_ir;
    let pic = Painter.paint_linear piet_ir in
    Printer.save_picture pic output_fname codel_dim;

  | (_,err_l) ->
    List.iter (fun z -> z |> Parser.error_msg |> print_string) err_l

let main_t = Term.(const main
                         $ loops_off $ cancel_off $ condense_off $ shrink_rolls
                         $ stack_size
                         $ ast_on
                         $ codel_dim $ output_fname $ input_fname $ input_str)

let _ = Term.exit @@ Term.eval (main_t, info)
