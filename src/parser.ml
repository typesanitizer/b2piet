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
let parse : string -> (bf_instr * pos) queue * error queue =
  fun s ->
    let pos_map (line_num, nl_pos, bfi_p_q, bracket_pos_l, err_q) i c =
      let make_pos = FilePos.make_pos in
      if c = '\n' then
        (line_num + 1, (i+1), bfi_p_q, bracket_pos_l, err_q)
      else
        let cur_pos = make_pos line_num nl_pos (i+1) in
        let push_elem = fun z -> Q.push (z, cur_pos) bfi_p_q in
        match BFInstr.char_to_instr c with
        | Some BFInstr.Loop ->
          let _ = push_elem BFInstr.Loop in
          (line_num, nl_pos, bfi_p_q, cur_pos :: bracket_pos_l, err_q)
        | Some BFInstr.Loopend -> (
            let _ = push_elem BFInstr.Loopend in
            match bracket_pos_l with
            | [] ->
              let _ = Q.push (UnmatchedBrackets cur_pos) err_q in
              (line_num, nl_pos, bfi_p_q, [], err_q)
            | _ :: tail ->
              (line_num, nl_pos, bfi_p_q, tail, err_q)
          )
        | Some x ->
          let _ = push_elem x in
          (line_num, nl_pos, bfi_p_q, bracket_pos_l, err_q)
        | _ -> (line_num, nl_pos, bfi_p_q, bracket_pos_l, err_q)
    in
    let fold_lefti = String.fold_lefti in
    let init = (1, 0, Q.create (), [], Q.create ()) in
    let (_, _, bfi_p_q, bracket_pos_l, err_q) = fold_lefti pos_map init s in
    let rec final_unmatched_brackets brktp_l errq =
      match brktp_l with
      | [] -> errq
      | h::t -> (let _ = Q.push (UnmatchedBrackets h) errq in
                 final_unmatched_brackets t errq) in
    (bfi_p_q, final_unmatched_brackets (List.rev bracket_pos_l) err_q)
