open Batteries
open Utils

type pos = FilePos.pos
type bf_instr = BFInstr.bf_instr

module Warning = struct
  type warning =
      IncrDecrSequence   of int * int * pos * pos (* sequence of + and - *)
    | LeftRightSequence  of int * int * pos * pos (* sequence of > and < *)
    | UselessBrackets    of pos * pos             (* presence of ][ *)
    | InfiniteLoopOrNoop of pos * pos             (* presence of [] *)

  let warn_msg =
    let sprintf = Printf.sprintf in
    let make = String.make in
    let pos_to_str = FilePos.pos_to_str in
    let sub_min (a:int) (b:int) = if a > b then (a-b,0) else (0, b-a) in
    function
    | IncrDecrSequence (n_incr,n_decr,lpos,rpos) ->
      let (new_n_incr, new_n_decr) = sub_min n_incr n_decr in
      sprintf
        ("Warning: contiguous sequence of increments and decrements.\n" ^^
         "Starting at %s and ending at %s.\n" ^^
         "Consider replacing it with %d+ %d- :\n" ^^ "%s\n")
        (pos_to_str lpos) (pos_to_str rpos) new_n_incr new_n_decr
        ((make new_n_incr '+') ^ (make new_n_decr '-'))
    | LeftRightSequence (n_left,n_right,lpos,rpos) ->
      let (new_n_left, new_n_right) = sub_min n_left n_right in
      sprintf
        ("Warning: contiguous sequence of left and right moves.\n" ^^
         "Starting at %s and ending at %s.\n" ^^
         "Consider replacing it with %d< %d> :\n" ^^ "%s\n")
        (pos_to_str lpos) (pos_to_str rpos) new_n_left new_n_right
        ((make new_n_left '<') ^ (make new_n_right '>'))
    | UselessBrackets (lpos,rpos) ->
      sprintf
        ("Warning: consecutive needless brackets ][.\n" ^^
         "Starting at %s and ending at %s.\n" ^^
         "Consider replacing ][ with an empty string.")
        (pos_to_str lpos) (pos_to_str rpos)
    | InfiniteLoopOrNoop (lpos,rpos) ->
      sprintf
        ("Warning: found []; it's either an infinite loop or a no-op.\n" ^^
         "Starting at %s and ending at %s.\n" ^^
         "Consider replacing [] with an empty string.")
        (pos_to_str lpos) (pos_to_str rpos)
end

include Warning

type 'a queue = 'a Queue.t
module Q = Queue

let parse_pos : string -> (bf_instr * pos) queue =
  fun s ->
    let pos_map (line_num, nl_pos, bfi_p_q) i c =
      (let make_pos = FilePos.make_pos in
       if c = '\n' then
         (line_num + 1, i, bfi_p_q)
       else match BFInstr.char_to_instr c with
         | Some x ->
           let _ = Q.push (x, make_pos line_num nl_pos i) bfi_p_q in
           (line_num, nl_pos, bfi_p_q)
         | _ -> (line_num, nl_pos, bfi_p_q))
    in
    let fold_lefti = String.fold_lefti in
    let (_, _, bfi_p_q) = fold_lefti pos_map (1, 0, Q.create ()) s in
    bfi_p_q
