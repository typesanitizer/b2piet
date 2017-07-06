open Batteries
open Utils

type pos = FilePos.pos
type bf_instr = BFInstr.bf_instr
type error = UnmatchedBracket of pos | LyingAboutLoops of pos

let error_msg =
  let sprintf = Printf.sprintf in
  let pos_to_str = FilePos.pos_to_str in
  function
  | UnmatchedBracket p ->
    sprintf "Error: Bracket at %s is unmatched.\n" (pos_to_str p)
  | LyingAboutLoops p ->
    sprintf "Error: A bracket is present at %s\
             but a `no loops flag was passed.\n" (pos_to_str p)

(** Parses Brainfuck code for correctness.

    Maintains a stack for brackets with positions (to report errors). Possible
    error cases are when one sees more ] than [, or there are some ['s left over
    at the end.
*)
let parse : string -> bool -> (bf_instr * pos) list * error list =
  fun s loops_off ->
    (* accumulators: line number, last newline position, and
       lists for bracket positions (stack-like) and return types *)
    let pos_map (line_num, nl_pos, bfi_p_l, bracket_pos_l, err_l) i c =
      let make_pos = FilePos.make_pos in
      if c = '\n' then
        (line_num + 1, (i+1), bfi_p_l, bracket_pos_l, err_l)
      else
        let cur_pos = make_pos line_num nl_pos (i+1) in
        let add z = (z, cur_pos) :: bfi_p_l in
        match BFInstr.char_to_instr c with
        | Some BFInstr.Loop ->
          if loops_off then
            (line_num, nl_pos, add BFInstr.Loop, cur_pos :: bracket_pos_l, LyingAboutLoops cur_pos :: err_l)
          else
            (line_num, nl_pos, add BFInstr.Loop, cur_pos :: bracket_pos_l, err_l)
        | Some BFInstr.Loopend ->
          (if loops_off then
             (line_num, nl_pos, add BFInstr.Loopend, cur_pos :: bracket_pos_l, LyingAboutLoops cur_pos :: err_l)
           else
             let new_bfi_p_l = add BFInstr.Loopend in
             match bracket_pos_l with
             | [] ->
               (line_num, nl_pos, new_bfi_p_l, [], UnmatchedBracket cur_pos :: err_l)
             | _ :: tail ->
               (line_num, nl_pos, new_bfi_p_l, tail, err_l))
        | Some x ->
          let new_bfi_p_l = add x in
          (line_num, nl_pos, new_bfi_p_l, bracket_pos_l, err_l)
        | _ -> (line_num, nl_pos, bfi_p_l, bracket_pos_l, err_l)
    in
    let init = (1, 0, [], [], []) in
    let (_, _, bfi_p_l, bracket_pos_l, err_l) =
      String.fold_lefti pos_map init s in
    let rec final_unmatched_brackets brktp_l errl =
      match brktp_l with
      | [] -> errl
      | h::t -> final_unmatched_brackets t  (UnmatchedBracket h :: errl)
    in let final_err_l = final_unmatched_brackets bracket_pos_l err_l in
    (List.rev bfi_p_l, List.rev final_err_l)
