open Batteries
module FP = Utils.FilePos
module BFI = Utils.BFInstr
type error = UnmatchedBracket of FP.t | LyingAboutLoops of FP.t

let error_msg =
  let sprintf = Printf.sprintf in
  function
  | UnmatchedBracket p ->
    sprintf "Error: Bracket at %s is unmatched.\n" (FP.pos_to_str p)
  | LyingAboutLoops p ->
    sprintf "Error: A bracket is present at %s\
             but a `no loops flag was passed.\n" (FP.pos_to_str p)

(** Parses Brainfuck code for correctness.

    Maintains a stack for brackets with positions (to report errors). Possible
    error cases are when one sees more ] than [, or there are some ['s left over
    at the end.
*)
let parse ?(loops_off = false) s =
    (* accumulators: line number, last newline position, and
       stacks for bracket positions and return types *)
    let f (line_num, nl_pos, bfi_p_l, bracket_pos_l, err_l) i c =
      if c = '\n' then
        (line_num + 1, (i+1), bfi_p_l, bracket_pos_l, err_l)
      else
        let cur_pos = FP.make_pos line_num nl_pos i in
        let add z = (z, cur_pos) :: bfi_p_l in
        match BFI.char_to_instr c with
        | Some BFI.Loop ->
          if loops_off then
            (line_num, nl_pos, add BFI.Loop, cur_pos :: bracket_pos_l,
             LyingAboutLoops cur_pos :: err_l)
          else
            (line_num, nl_pos, add BFI.Loop, cur_pos :: bracket_pos_l, err_l)
        | Some BFI.Loopend ->
          (if loops_off then
             (line_num, nl_pos, add BFI.Loopend, cur_pos :: bracket_pos_l,
              LyingAboutLoops cur_pos :: err_l)
           else
             let new_bfi_p_l = add BFI.Loopend in
             match bracket_pos_l with
             | [] ->
               (line_num, nl_pos, new_bfi_p_l, [],
                UnmatchedBracket cur_pos :: err_l)
             | _ :: tail ->
               (line_num, nl_pos, new_bfi_p_l, tail, err_l))
        | Some x ->
          let new_bfi_p_l = add x in
          (line_num, nl_pos, new_bfi_p_l, bracket_pos_l, err_l)
        | _ -> (line_num, nl_pos, bfi_p_l, bracket_pos_l, err_l)
    in
    let init = (1, 0, [], [], []) in
    let (_, _, bfi_p_l, bracket_pos_l, err_l) =
      String.fold_lefti f init s in
    let rec final_unmatched_brackets brktp_l errl =
      List.fold_left (fun a x -> UnmatchedBracket x :: a) errl brktp_l
    in let final_err_l = final_unmatched_brackets bracket_pos_l err_l in
    (List.rev bfi_p_l, List.rev final_err_l)
