open Batteries
open Utils.BFInstr
open Utils.FilePos

module Warning = struct
  type warning =
      IncrDecrSequence   of pos * pos (* sequence of + and - *)
    | LeftRightSequence  of pos * pos (* sequence of > and < *)
    | UselessBrackets    of pos * pos (* presence of ][ *)
    | InfiniteLoopOrNoop of pos * pos (* presence of [] *)

  let warn_msg =
    let sprintf = Printf.sprintf in
    function
    | IncrDecrSequence (lpos,rpos) ->
      sprintf
        ("Warning: contiguous sequence of increments and decrements.\n" ^^
         "    Starting at %s and ending at %s.\n" ^^
         "    Consider replacing it with an empty string.\n")
        (pos_to_str lpos) (pos_to_str rpos)
    | LeftRightSequence (lpos,rpos) ->
      sprintf
        ("Warning: contiguous sequence of left and right moves.\n" ^^
         "    Starting at %s and ending at %s.\n" ^^
         "    Consider replacing it with with an empty string.\n")
        (pos_to_str lpos) (pos_to_str rpos)
    | UselessBrackets (lpos,rpos) ->
      sprintf
        ("Warning: consecutive needless brackets ][.\n" ^^
         "    Starting at %s and ending at %s.\n" ^^
         "    Consider replacing ][ with an empty string.\n")
        (pos_to_str lpos) (pos_to_str rpos)
    | InfiniteLoopOrNoop (lpos,rpos) ->
      sprintf
        ("Warning: found []; it's either an infinite loop or a no-op.\n" ^^
         "    Starting at %s and ending at %s.\n")
        (pos_to_str lpos) (pos_to_str rpos)
end

include Warning

(*
   The first argument is emptied and an optimal sequence is created in the
   second list. The warnings are returned. NOTE: the optimal sequence list
   returns instructions in reversed order.

   The ends of IncrDecrSequence and LeftRightSequence are guaranteed to have an
   equal number of +'s (<'s) and -'s (>'s) between them (including ends).
*)
let rec optimise_rev :
  (bf_instr * pos) list * (bf_instr * pos) list * warning list ->
  (bf_instr * pos) list * (bf_instr * pos) list * warning list
  = function
    | ((Incr,rp)::t, (Decr,lp)::s, wl)
    | ((Decr,rp)::t, (Incr,lp)::s, wl) ->
      let new_wl = match wl with
        (* ++-- lp2 >. lp (nesting), +-+- lp2 <. lp (sequence) *)
        | IncrDecrSequence (lp2,_) :: r  ->
          if lp2 >. lp then IncrDecrSequence (lp,rp) :: r
          else IncrDecrSequence (lp2, rp) :: r
        | _ -> IncrDecrSequence (lp,rp) :: wl in
      optimise_rev (t, s, new_wl)
    | ((Left,rp)::t, (Right,lp)::s, wl)
    | ((Right,rp)::t, (Left,lp)::s, wl) ->
      let new_wl = match wl with
        (* <<>> lp2 >. lp (nesting), <><> lp2 <. lp (sequence) *)
        | LeftRightSequence (lp2,_) :: r  ->
          if lp2 >. lp then LeftRightSequence (lp,rp) :: r
          else LeftRightSequence (lp2, rp) :: r
        | _ -> LeftRightSequence (lp,rp) :: wl in
      optimise_rev (t, s, new_wl)
    | ((Loop,rp)::t, (Loopend,lp)::s, wl) ->
      let new_wl = match wl with
        (* ]][[ lp2 >. lp (nesting), ][][ lp2 <. lp (sequence) *)
        | UselessBrackets (lp2,_) :: r ->
          if lp2 >. lp then UselessBrackets (lp,rp) :: r
          else UselessBrackets (lp2, rp) :: r
        | _ -> UselessBrackets (lp,rp) :: wl in
      optimise_rev (t, s, new_wl)
    | ((Loopend,rp)::t, (Loop,lp)::s, wl) ->
      let new_wl = InfiniteLoopOrNoop (lp,rp) :: wl in
      optimise_rev (t, (Loopend,rp)::(Loop,lp)::s, new_wl)
    | (h::t,a,w) -> optimise_rev (t,h::a,w)
    | ([],a,w) -> ([],a,w)

let optimise bfi_p_l =
  let (_,new_bfi_p_l,warnings) = optimise_rev (bfi_p_l,[],[]) in
  (List.rev new_bfi_p_l, List.rev warnings)
