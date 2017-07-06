open Batteries
open Utils.BFInstr
open Utils.FilePos
open Utils.PietIR

type ir = Utils.PietIR.ir
module V = BatVect
type 'a vec = 'a V.t

type cell_pos = int
type oob = L of pos | R of pos
type runtime_err = OutOfBounds of oob | LoopPresent

(* (Huge?) Record for recursive translation *)
type trex = {
  bfip    : (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list;
  ir_l    : ir list;
  stackv  : cell_pos vec;       (* vector representation of stack*)
  cp      : cell_pos;           (* current cell position *)
  depth   : int;                (* depth of brackets *)
  run_err : runtime_err option; (* halts at first error *)
  cond    : bool;               (* true => code should be condensed *)
  loops   : bool;               (* true => code has loops *)
  ssize   : int;                (* Piet stack size for loopy programs *)
}

(* Bubble only scheme for translation of linear code *)
let bubble_bb_only ir_l stackv cp =
  if V.last stackv = cp then (ir_l, stackv)
  else try
      let x = V.findi ((=) cp) stackv in
      let y = V.length stackv - x in
      match ir_l with
      | Roll (1,y) :: r -> (ir_l, stackv)
      | _ -> (Roll (-1,y) :: ir_l, stackv)
    with Not_found ->
      (Push 0 :: ir_l, V.append cp stackv)

let prep_for_use_bb_only trec =
  match trec with
  | {ir_l; stackv; cp} ->
    if V.is_empty stackv then
      {trec with ir_l = Push 0 :: ir_l; stackv = V.singleton cp}
    else
      let (new_ir_l, new_stackv) = bubble_bb_only ir_l stackv cp in
      {trec with ir_l = new_ir_l; stackv = new_stackv;}

let prep_for_use trec =
  if trec.loops = true then trec
  else prep_for_use_bb_only trec

(* Bubble bury scheme for translation of linear code *)
(* let bubble ir_l stackv cp = *)
(*   if V.last stackv = cp then (ir_l, stackv) *)
(*   else try *)
(*       let x = V.findi ((=) cp) stackv in *)
(*       let y = V.length stackv - x in *)
(*       match ir_l with *)
(*       | Roll (1,y) :: r -> (ir_l, stackv) *)
(*       | _ -> (Roll (-1,y) :: ir_l, stackv) *)
(*     with Not_found -> *)
(*       (Push 0 :: ir_l, V.append cp stackv) *)

(* let bury ir_l stackv cp = *)
(*   if V.last stackv = cp then ir_l *)
(*   else try *)
(*       let x = V.findi ((=) cp) stackv in *)
(*       let y = V.length stackv - x in *)
(*       Roll (1,y) :: ir_l *)
(*     with Not_found -> *)
(*       let _ = print_endline "Bug!!!" in *)
(*       ir_l *)

(* (\* Shared for linear and loopy code *\) *)
(* let prep_for_use trec = *)
(*   if trec.loops then trec *)
(*   else match trec with *)
(*     | {ir_l; stackv; cp} -> *)
(*       if V.is_empty stackv then *)
(*         {trec with ir_l = Push 0 :: ir_l; stackv = V.singleton cp} *)
(*       else *)
(*         let (new_ir_l, new_stackv) = bubble ir_l stackv cp in *)
(*         {trec with ir_l = new_ir_l; stackv = new_stackv;} *)

(* let clean_after_use trec = *)
(*   if trec.loops then trec *)
(*   else match trec with *)
(*     | {ir_l; stackv; cp} -> *)
(*       let new_ir_l = bury ir_l stackv cp in *)
(*       {trec with ir_l = new_ir_l;} *)

(*
   Translates the list of instructions to Piet IR.

   NOTE: Output is reversed.
   NOTE: DO NOT replace the laziness for `default` with strictness. Strictness
   considerably slows down processing when you have a sequence of ten or more
   +'s or -'s.
*)

let rec translate_rev : trex -> trex
  =
  fun trec ->
    match trec with

    (* No loop case *)
    | {bfip = (Left,p)::t; cp = 0; loops = false} ->
      {trec with run_err = Some (OutOfBounds (L p));}

    | {bfip = (Right,p)::t; cp = 29999; loops = false} ->
      {trec with run_err = Some (OutOfBounds (R p));}

    | {bfip = (Left,_)::t; cp; loops = false} ->
      translate_rev {trec with bfip = t; cp = cp - 1;}

    | {bfip = (Right,_)::t; cp; loops = false} ->
      translate_rev {trec with bfip = t; cp = cp + 1; loops = false}

    | {bfip = (Loop,p)::t; ir_l = cur_ir_l; depth; loops = false} ->
      translate_rev {trec with run_err = Some LoopPresent}

    | {bfip = (Loopend,p)::t; ir_l = cur_ir_l; depth; loops = false} ->
      translate_rev {trec with run_err = Some LoopPresent}

    (* Movement when loops are present*)
    | {bfip = (Left,_)::t; ir_l; cond; loops = true} ->
      let default =
        lazy (translate_rev
                {trec with bfip = t; ir_l = Roll (1,trec.ssize) :: ir_l;}) in
      if cond then match ir_l with
        | Roll (a, sz) :: r ->
          translate_rev {trec with bfip = t; ir_l = Roll (a+1,sz) :: r;}
        | _ -> Lazy.force default
      else Lazy.force default

    | {bfip = (Right,_)::t; ir_l; cond; loops = true} ->
      let default =
        lazy (translate_rev
                {trec with bfip = t; ir_l = Roll (-1,trec.ssize) :: ir_l;}) in
      if cond then match ir_l with
        | Roll (a, sz) :: r ->
          translate_rev {trec with bfip = t; ir_l = Roll (a-1,sz) :: r;}
        | _ -> Lazy.force default
      else Lazy.force default

    | {bfip = (Loop,_)::t; ir_l; depth; loops = true} ->
      let trec = prep_for_use trec in
      let trec = translate_rev
          {trec with bfip=t; ir_l = []; depth=depth+1;} in
      translate_rev {trec with ir_l = Loop trec.ir_l :: ir_l}

    | {bfip = (Loopend,_)::t; depth; loops = true} ->
      let trec = prep_for_use trec in
      {trec with bfip=t; depth=depth-1;}

    (* Common operations*)
    | {bfip = (Incr,_)::t; cond;} ->
      (let trec = {(prep_for_use trec) with bfip = t;} in
       let ir_l = trec.ir_l in
       let default =
         lazy (translate_rev {trec with ir_l = Mod 256 :: Add 1 :: ir_l}) in
       if cond then match ir_l with
         | Mod 256 :: Add 255 :: r ->
           translate_rev {trec with ir_l = r;}
         | Push a :: r | Mod 256 :: Push a :: r ->
           translate_rev {trec with ir_l = Mod 256 :: Push (a+1) :: r}
         | Mod 256 :: Add a :: r ->
           translate_rev {trec with ir_l = Mod 256 :: Add (a+1) :: r}
         | _ -> Lazy.force default
       else Lazy.force default)
      (* |> clean_after_use *)

    | {bfip = (Decr,_)::t; cond;} ->
      (let trec = prep_for_use trec in
       let ir_l = trec.ir_l in
       let default = lazy
         (translate_rev
            {trec with bfip = t; ir_l = Mod 256 :: Subtract 1 :: ir_l}) in
       if cond then match ir_l with
         | Mod 256 :: Subtract 255 :: r ->
           translate_rev {trec with bfip = t; ir_l = r;}
         | Push a :: r | Mod 256 :: Push a :: r ->
           translate_rev
             {trec with bfip = t; ir_l = Mod 256 :: Push (a-1) :: r}
         | Mod 256 :: Subtract a :: r ->
           translate_rev
             {trec with bfip = t; ir_l = Mod 256 :: Subtract (a+1) :: r}
         | _ -> Lazy.force default
       else Lazy.force default)
      (* |> clean_after_use *)

    | {bfip = (Input,_)::t; cond; ir_l;} ->
      (let trec = prep_for_use trec in
       let ir_l = trec.ir_l in
       let default = lazy
         (translate_rev
            {trec with bfip = t; ir_l = Mod 256 :: Input :: ir_l;}) in
       if cond then match ir_l with
         | Mod 256 :: Add _ :: r | Mod 256 :: Subtract _ :: r ->
           translate_rev {trec with bfip = t; ir_l = Input :: r;}
         | _ -> Lazy.force default
       else Lazy.force default)

    | {bfip = (Output,_)::t; ir_l;} ->
      let trec = prep_for_use trec in
      translate_rev {trec with bfip = t; ir_l = Output :: ir_l;}

    | {bfip = []; ir_l;} ->
      {trec with ir_l = Eop :: ir_l}

let rec rev_tree acc tr =
  let rec rev_tree_f acc = function
    | Loop tr -> Loop (rev_tree [] tr) :: acc
    | x -> x :: acc in
  List.fold_left rev_tree_f [] tr

(*
   NOTE: Output is reversed.

   acc @ left = rev initial, right @ middle = rev left

                   middle        right      H
            ~~~~~~~~~~~~~~~~~   +++++++++   E
     acc     left                           A
    @@@@@   -----------------------------   D
    0   1   2   3   4   5   6   7   8   9
*)
let rec shrink_rolls :
  (ir list * ir list * ir list * ir list) ->
  (ir list * ir list * ir list * ir list) =
  fun  (l_subtree,r_subtree,middle,acc) ->
    match l_subtree, r_subtree, middle with
    | (lt::l, rh::r, []) ->
      shrink_rolls (lt::l, r, [lt], acc)
    | (Roll (a,b)::l, Roll (c,d)::r, _) ->
      if a = -c && b = d then
        let new_d = min d @@ abs c in
        (* No exceptions handling: last element is Roll (a,b) *)
        let middle_remove_last = middle |> List.rev |> List.tl |> List.rev in
        shrink_rolls (r, r, [],
                      Roll (c, new_d)::middle_remove_last @ Roll (a,new_d)::acc)
      else
        shrink_rolls (Roll (c,d)::r, r, [Roll (c,d)], middle @ acc)
    | (Roll (a,b)::l, Loop tr::r, _) ->
      let (_,_,_,subtree) = shrink_rolls (tr,tr,[],[]) in
      shrink_rolls (r, r, [], Loop subtree:: Roll (a,b) :: acc)
    | (Roll (a,b)::l, rh::r, _) ->
      shrink_rolls (Roll (a,b)::l, r, rh::middle, acc)
    | (Roll (a,b)::l, [], _) -> ([], [], [], middle @ acc)
    | (lh::l, _, _) ->
      shrink_rolls (l, l, [], middle @ acc)
    | ([], _, _) -> ([],[],[], acc)

(* NOTE: Output is reversed. *)
let they_see_me_rollin_they_hatin ir_tree =
  let (_,_,_,small_tree) = shrink_rolls (ir_tree, ir_tree, [], [])
  in small_tree

let translate bfip_l condense loops_present stack_size shrink_rolls =
  let ir_l = if loops_present then List.make stack_size (Push 0) else [] in
  let trec = translate_rev {
      bfip = bfip_l;
      ir_l;
      stackv = V.empty;
      cp = 0;
      depth = 0;
      run_err = None;
      cond = condense;
      loops = loops_present;
      ssize = stack_size;
    } in
  (if shrink_rolls then (they_see_me_rollin_they_hatin trec.ir_l, trec.run_err)
   else rev_tree [] trec.ir_l, trec.run_err)
