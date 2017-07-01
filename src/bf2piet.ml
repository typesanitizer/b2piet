open Cmdliner
open Utils.FilePos
open Utils.BFInstr
open Parser

let ast_on =
  let doc = "Print the Piet IR tree generated after translation." in
  Arg.(value & flag & info ["i"; "ir-tree"] ~doc)
let loops_off =
  let doc = "Make drawings based using the fact that there are no loops." in
  Arg.(value & flag & info ["l"; "linear"; "no-loops"] ~doc)
let cancel_off =
  let doc = "Turn off cancellations \\(fusion of \\(+,-\\), \\(>,<\\), \
             deleting \\(][\\)\\). Also disables related warnings." in
  Arg.(value & flag & info ["c"; "no-cancel"] ~doc)
let condense_off =
  let doc = "Turn off condensation. Piet will perform additions and \
             subtractions one at a time." in
  Arg.(value & flag & info ["C"; "no-condense"] ~doc)
let shrink_rolls =
  let doc = "[Experimental] Try to reduce size of roll operations for \
             Piet output." in
  Arg.(value & flag & info ["S"; "shrink"] ~doc)
let stack_size =
  let doc = "The size of the Piet stack for operation. For correctness \
             in programs with loops, this must be set to a value greater than \
             the value of the maximum cell number (starting at zero) reached \
             by the Brainfuck input. This size is ignored if the no-loops \
             argument is given." in
  Arg.(value & opt int 8 & info ["stack"] ~doc)
let input_str =
  let doc = "The Brainfuck code to be transpiled." in
  Arg.(value & pos 0 string "+" & info [] ~docv:"STRING" ~doc)
let info =
  let doc = "Convert Brainfuck programs into Piet programs." in
  let man = [
    `S   Manpage.s_examples;
    `P   "Basic usage:";
    `Pre "    \\$ ./bf2piet.byte \"+++[>+<-].\"";
    `P   "Using code from a .bf file:";
    `Pre "    \\$ ./bf2piet.byte \"\\$\\(cat madoka.bf\\)\"";
    `S Manpage.s_bugs;
    `P ("Open an issue on https://github.com/theindigamer/bf-piet/ or send an \
         email to `theindigamer15 AT gmail DOT com`.")
  ]
  in Term.info "bf2piet" ~doc ~exits:Term.default_exits ~man

let main cancel_off condense_off ast_on loops_off stack_size shrink_rolls str =
  let cancel = not cancel_off in
  let condense = not condense_off in
  let loops = not loops_off in
  match parse str loops_off with
  | (bfip_l, []) ->
    let bfip_l2 =
      if cancel then
        let (bfip, warn_l) = Optimiser.optimise bfip_l in
        let _ = List.map (fun z -> z |> Optimiser.warn_msg |> print_string) warn_l in
        bfip
      else bfip_l
    in
    let (piet_ir, err) =
      Translator.translate bfip_l2 condense loops stack_size shrink_rolls in
    let _ = if ast_on then Translator.print_ast piet_ir else [()] in
    ()
  | (_,err_l) ->
    let _ = List.map (fun z -> z |> error_msg |> print_string) err_l in
    ()

let main_t = Term.(const main
                   $ cancel_off $ condense_off $ ast_on $ loops_off $ stack_size
                   $ shrink_rolls
                   $ input_str)
let _ = Term.exit @@ Term.eval (main_t, info)
