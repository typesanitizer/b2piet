open Utils.FilePos
open Utils.BFInstr
open Parser
open Optimiser

let str = "+++>><><<---\n++\n-- +++>>>["
let opt = true

let main () =
  match parse str with
  | (bfip_l, []) ->
    if opt then
      let (bfip_l, warn_l) = optimise bfip_l in
      let _ = List.map (fun z -> z |> warn_msg |> print_string) warn_l in
      ()
    else
      let _ = List.map
          (fun (bfi,p) ->
             Printf.printf "%c %s\n" (instr_to_char bfi) (pos_to_str p))
          bfip_l in
      ()
  | (_,err_l) ->
    let _ = List.map (fun z -> z |> error_msg |> print_string) err_l in
    ()

let _ = main ()
