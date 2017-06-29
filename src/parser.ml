open Batteries
open Utils

type pos = FilePos.pos
type bf_instr = BFInstr.bf_instr
type error = UnmatchedBrackets of pos

type 'a queue = 'a Queue.t
module Q = Queue

let error_msg =
  let sprintf = Printf.sprintf in
  let pos_to_str = FilePos.pos_to_str in
  function
  | UnmatchedBrackets p ->
    sprintf "Error: Bracket at %s is unmatched.\n" (pos_to_str p)

(** Parses Brainfuck code for correctness.

    Maintains a stack for brackets with positions (to report errors). Possible
    error cases are when one sees more ] than [, or there are some ['s left over
    at the end.
*)
let parse : string -> (bf_instr * pos) list * error list =
  fun s ->
    let pos_map (line_num, nl_pos, bfi_p_l, bracket_pos_l, err_l) i c =
      let make_pos = FilePos.make_pos in
      if c = '\n' then
        (line_num + 1, (i+1), bfi_p_l, bracket_pos_l, err_l)
      else
        let cur_pos = make_pos line_num nl_pos (i+1) in
        let add z = (z, cur_pos) :: bfi_p_l in
        match BFInstr.char_to_instr c with
        | Some BFInstr.Loop ->
          (line_num, nl_pos, add BFInstr.Loop, cur_pos :: bracket_pos_l, err_l)
        | Some BFInstr.Loopend ->
          (let new_bfi_p_l = add BFInstr.Loopend in
           match bracket_pos_l with
           | [] ->
             (line_num, nl_pos, new_bfi_p_l, [], UnmatchedBrackets cur_pos :: err_l)
           | _ :: tail ->
             (line_num, nl_pos, new_bfi_p_l, tail, err_l))
        | Some x ->
          let new_bfi_p_l = add x in
          (* let _ = push_elem x in *)
          (line_num, nl_pos, new_bfi_p_l, bracket_pos_l, err_l)
        | _ -> (line_num, nl_pos, bfi_p_l, bracket_pos_l, err_l)
    in
    let fold_lefti = String.fold_lefti in
    let init = (1, 0, [], [], []) in
    let (_, _, bfi_p_l, bracket_pos_l, err_l) = fold_lefti pos_map init s in
    let rec final_unmatched_brackets brktp_l errl =
      match brktp_l with
      | [] -> errl
      | h::t -> final_unmatched_brackets t  (UnmatchedBrackets h :: errl)
    in let final_err_l = final_unmatched_brackets bracket_pos_l err_l in
    (List.rev bfi_p_l, List.rev final_err_l)
