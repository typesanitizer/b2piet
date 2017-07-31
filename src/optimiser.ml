open Batteries
module FP = Utils.FilePos

module Warning = struct
  type warning =
      IncrDecrSequence   of FP.t * FP.t (* sequence of + and - *)
    | LeftRightSequence  of FP.t * FP.t (* sequence of > and < *)
    (* | UselessBrackets    of FP.t * FP.t (\* presence of ][ *\) *)
    | InfiniteLoopOrNoop of FP.t * FP.t (* presence of [] *)

  let warn_msg w =
    let (str, lpos, rpos) = match w with
      | IncrDecrSequence (lpos,rpos) ->
        ("Warning: contiguous sequence of increments and decrements.\n" ^^
         "    Starting at %s and ending at %s.\n" ^^
         "    Consider replacing it with an empty string.\n",
         lpos, rpos)
      | LeftRightSequence (lpos,rpos) ->
        ("Warning: contiguous sequence of left and right moves.\n" ^^
         "    Starting at %s and ending at %s.\n" ^^
         "    Consider replacing it with with an empty string.\n",
         lpos, rpos)
      (* | UselessBrackets (lpos,rpos) -> *)
      (*   ("Warning: consecutive needless brackets ][.\n" ^^ *)
      (*    "    Starting at %s and ending at %s.\n" ^^ *)
      (*    "    Consider replacing ][ with an empty string.\n", *)
      (*    lpos, rpos) *)
      | InfiniteLoopOrNoop (lpos,rpos) ->
        ("Warning: found []; it's either an infinite loop or a no-op.\n" ^^
         "    Starting at %s and ending at %s.\n"
        , lpos, rpos) in
    Printf.sprintf str (FP.pos_to_str lpos) (FP.pos_to_str rpos)
end

include Warning

(*
   The first argument is emptied and an optimal sequence is created in the
   second list. The warnings are returned. NOTE: the optimal sequence list
   returns instructions in reversed order.

   The ends of IncrDecrSequence and LeftRightSequence are guaranteed to have an
   equal number of +'s (<'s) and -'s (>'s) between them (including ends).
*)
let rec f (acc_bfipl, acc_wl) (instr, p) =
  let open Utils.BFInstr in
  match instr, p, acc_bfipl, acc_wl with
  | Incr, rp, (Decr, lp) :: s, wl
  | Decr, rp, (Incr, lp) :: s, wl ->
    let new_wl = match wl with
      (* ++-- lp2 >. lp (nesting), +-+- lp2 <. lp (sequence) *)
      | Some (IncrDecrSequence (lp2, _)) :: r  ->
        if FP.(lp2 >. lp) then Some (IncrDecrSequence (lp, rp)) :: r
        else Some (IncrDecrSequence (lp2, rp)) :: r
      | _ -> Some (IncrDecrSequence (lp, rp)) :: wl in
    (s, new_wl)
  | Left,  rp, (Right, lp) :: s, wl
  | Right, rp, (Left, lp) :: s, wl ->
    let new_wl = match wl with
      (* <<>> lp2 >. lp (nesting), <><> lp2 <. lp (sequence) *)
      | Some (LeftRightSequence (lp2,_)) :: r  ->
        if FP.(lp2 >. lp) then Some (LeftRightSequence (lp, rp)) :: r
        else Some (LeftRightSequence (lp2, rp)) :: r
      | _ -> Some (LeftRightSequence (lp, rp)) :: wl in
    (s, new_wl)
  (* | Loop, rp, (Loopend, lp) :: s, wl -> *)
  (*   let new_wl = match wl with *)
  (*     (\* ]][[ lp2 >. lp (nesting), ][][ lp2 <. lp (sequence) *\) *)
  (*     | UselessBrackets (lp2, _) :: r -> *)
  (*       if FP.(lp2 >. lp) then UselessBrackets (lp, rp) :: r *)
  (*       else UselessBrackets (lp2, rp) :: r *)
  (*     | _ -> UselessBrackets (lp, rp) :: wl in *)
  (*   (s, new_wl) *)
  | Loopend, rp, (Loop,lp) :: s, wl ->
    let new_wl = Some (InfiniteLoopOrNoop (lp, rp)) :: wl in
    ((Loopend,rp)::(Loop,lp) :: s, new_wl)
  | hi, hp, a, w -> match w with
    | None :: tw -> ((hi, hp) :: a, w)
    | _ -> ((hi, hp) :: a, None :: w)


let optimise bfi_p_l =
  let (new_bfi_p_l,warnings) = List.fold_left f ([],[]) bfi_p_l in
  (List.rev new_bfi_p_l, List.rev % List.map BatOption.get
     % List.filter BatOption.is_some @@ warnings)
