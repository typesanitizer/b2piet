open Batteries
module FP = Utils.FilePos
module BFI = Utils.BFInstr
module PI = Utils.PietIR
type ir = Utils.PietIR.ir
module V = BatVect

type cell_pos = int
type oob = L of FP.t | R of FP.t
type runtime_err = OutOfBounds of oob | LoopPresent
type interpret_err = OutOfBounds of oob | InputPresent

let interpret_err_msg =
  let sprintf = Printf.sprintf in
  function
  | OutOfBounds (L p) -> sprintf "OOB L %s\n" (FP.pos_to_str p)
  | OutOfBounds (R p) -> sprintf "OOB R %s\n" (FP.pos_to_str p)
  | InputPresent -> "IP\n"

type interpret_rec = {
  outstr : Buffer.t;
  maxdp  : int;
  dp     : int;
  ip     : int;
  tape   : int array;
  bfip_a : (BFI.t * FP.t) array;
  len    : int;
}

let interpret_tuple i = (i.outstr, i.maxdp, i.dp)
let interpret_tuple_werr i err = (i.outstr, i.maxdp, i.dp, err)

let interpret_main output bfip_l =
  let (>>=) = Lwt.Infix.(>>=) in
  let pause z = Lwt_main.yield () >>= fun () -> z in
  let next i = {i with ip = i.ip + 1} in
  let rec f i =
    if i.ip = i.len then
      Lwt.return @@ interpret_tuple_werr i None
    else
      match Array.get i.bfip_a i.ip with
      | BFI.Input, _ ->
        Lwt.return @@ interpret_tuple_werr i (Some InputPresent)
      | BFI.Output, _ ->
        if output then
          Buffer.add_char i.outstr @@ char_of_int (Array.get i.tape i.dp);
        pause @@ f (next i)
      | BFI.Incr, _ ->
        let x = Array.get i.tape i.dp in
        Array.set i.tape i.dp (if x = 255 then 0 else (x + 1));
        pause @@ f (next i)
      | BFI.Decr, _ ->
        let x = Array.get i.tape i.dp in
        Array.set i.tape i.dp (if x = 0 then 255 else (x - 1));
        pause @@ f (next i)
      | BFI.Left, p ->
        if i.dp = 0 then
          Lwt.return @@ interpret_tuple_werr i (Some (OutOfBounds (L p)))
        else pause @@ f {i with dp = i.dp - 1; ip = i.ip + 1}
      | BFI.Right, p ->
        if i.dp = 29999 then
          Lwt.return @@ interpret_tuple_werr i (Some (OutOfBounds (R p)))
        else pause @@ f {
            i with maxdp = max i.maxdp (i.dp + 1);
                   dp = i.dp + 1;
                   ip = i.ip + 1;
          }
      | BFI.Loopend, p ->
        raise (Failure "Unexpected Loopend. Should've been jumped across.")
      | BFI.Loop, p ->
        let rec find_loopend bfip_a cur count =
          match Array.get bfip_a cur with
          | BFI.Loop, _ -> find_loopend bfip_a (cur + 1) (count + 1)
          | BFI.Loopend, _ ->
            if count = 0 then cur
            else find_loopend bfip_a (cur + 1) (count - 1)
          | _ -> find_loopend bfip_a (cur + 1) count
        in
        let loopend_n = find_loopend i.bfip_a (i.ip + 1) 0 in
        let innerlen = loopend_n - (i.ip + 1) in
        let inner = Array.sub i.bfip_a (i.ip + 1) innerlen in
        let rec loop (outstr, maxdp, dp) = match Array.get i.tape dp with
          | 0 -> Lwt.return (outstr, maxdp, dp, None)
          | _ ->
            let tmp = f {
                outstr;
                maxdp;
                dp;
                ip = 0;
                tape = i.tape;
                bfip_a = inner;
                len = innerlen;
              } in
            let go (outstr, maxdp, dp, err) =
              match err with
              | None -> pause @@ loop (outstr, maxdp, dp)
              | Some err -> Lwt.return (outstr, maxdp, dp, Some err) in
            tmp >>= go
        in
        let tmp = loop @@ interpret_tuple i (* (outstr, maxdp, dp) *) in
        let go (outstr, maxdp, dp, err) =
          match err with
          | None -> pause @@
            f {i with outstr; maxdp; dp; ip = (loopend_n + 1);}
          | Some err -> Lwt.return (outstr, maxdp, dp, Some err) in
        tmp >>= go
  in
  let bfip_a = Array.of_list bfip_l in
  let len = Array.length bfip_a in
  let tape = Array.make 30000 0 in
  Lwt.map (fun (outstr, maxdp, _, err) ->
      (maxdp + 1, Buffer.contents outstr, err))
  @@ f {
    outstr = Buffer.create 4096;
    maxdp = 0;
    dp = 0;
    ip = 0;
    tape;
    bfip_a;
    len;
  }

let interpret i = Lwt_main.run (interpret_main false i) |> fun (a, _, b) -> (a, b)
let interpret_woutput = interpret_main true

(* (Huge?) Record for recursive translation *)
type trex = {
  bfip    : (BFI.t * FP.t) list;
  ir_l    : ir list;
  stackv  : cell_pos V.t;       (* vector representation of stack *)
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
      | PI.Roll (1,y) :: r -> (ir_l, stackv)
      | _ -> (PI.Roll (-1, y) :: ir_l, stackv)
    with Not_found ->
      (PI.Push 0 :: ir_l, V.append cp stackv)

let prep_for_use_bb_only trec =
  match trec with
  | {ir_l; stackv; cp} ->
    if V.is_empty stackv then
      {trec with ir_l = PI.Push 0 :: ir_l; stackv = V.singleton cp}
    else
      let (new_ir_l, new_stackv) = bubble_bb_only ir_l stackv cp in
      {trec with ir_l = new_ir_l; stackv = new_stackv;}

let prep_for_use trec =
  if trec.loops = true then trec
  else prep_for_use_bb_only trec

let optimise_loop ssize ir_l =
  if List.length ir_l >= 1 then
    if List.last ir_l = PI.Subtract 1 then
      let module Piet = Utils.Piet in
      let ir_l_rev = List.rev ir_l in (* actually correct order *)
      let rec go acc n = function
        | PI.Roll (m, ssize) :: t -> go acc (n + m) t
        | PI.Add a :: t -> (match compare n 0 with
            | x when x > 0 ->
              go (PI.Roll (1, n + 1) :: PI.Op (Piet.PAdd)
                  :: PI.Roll (-1, n + 2) :: PI.Multiply a
                  :: PI.Op (Piet.PDup) :: acc) n t
            | x when x < 0 ->
              go (PI.Roll (1, ssize + n + 1) :: PI.Op (Piet.PAdd)
                  :: PI.Roll (-1, ssize + n + 2) :: PI.Multiply a
                  :: PI.Op (Piet.PDup) :: acc) n t
            | _  -> (acc, n, true))
        | [] -> (acc, n, false)
        | _ -> (acc, n, true)
      in
      let (acc, n, fail) = go [] 0 (List.tl ir_l_rev) in
      if fail || n <> 0 then [PI.Loop ir_l]
      else PI.Op Piet.PSub :: PI.Op Piet.PDup :: acc
    else
      [PI.Loop ir_l]
  else
    [PI.Loop ir_l]

(*
   Translates the list of instructions to Piet IR.

   NOTE: Output is reversed.
   NOTE: DO NOT replace the laziness for `default` with strictness. Strictness
   considerably slows down processing when you have a sequence of ten or more
   +'s or -'s.
*)
let rec translate_rev =
  fun trec ->
    match trec with

    (* No loop case *)
    | {bfip = (BFI.Left, p) :: t; cp = 0; loops = false} ->
      {trec with run_err = Some (OutOfBounds (L p));}

    | {bfip = (BFI.Right, p) :: t; cp = 29999; loops = false} ->
      {trec with run_err = Some (OutOfBounds (R p));}

    | {bfip = (BFI.Left, _) :: t; cp; loops = false} ->
      translate_rev {trec with bfip = t; cp = cp - 1;}

    | {bfip = (BFI.Right,_) :: t; cp; loops = false} ->
      translate_rev {trec with bfip = t; cp = cp + 1; loops = false}

    | {bfip = (BFI.Loop, p) :: t; ir_l = cur_ir_l; depth; loops = false} ->
      translate_rev {trec with run_err = Some LoopPresent}

    | {bfip = (BFI.Loopend, p) :: t; ir_l = cur_ir_l; depth; loops = false} ->
      translate_rev {trec with run_err = Some LoopPresent}

    (* Movement when loops are present *)
    | {bfip = (BFI.Left, _) :: t; ir_l; cond; loops = true} ->
      let default =
        lazy (translate_rev
                {trec with bfip = t;
                           ir_l = PI.Roll (1, trec.ssize) :: ir_l;}) in
      if cond then match ir_l with
        | PI.Roll (a, sz) :: r ->
          translate_rev {trec with bfip = t; ir_l = PI.Roll (a + 1, sz) :: r;}
        | _ -> Lazy.force default
      else Lazy.force default

    | {bfip = (BFI.Right, _) :: t; ir_l; cond; loops = true} ->
      let default =
        lazy (translate_rev
                {trec with bfip = t;
                           ir_l = PI.Roll (-1, trec.ssize) :: ir_l;}) in
      if cond then match ir_l with
        | PI.Roll (a, sz) :: r ->
          translate_rev {trec with bfip = t; ir_l = PI.Roll (a - 1, sz) :: r;}
        | _ -> Lazy.force default
      else Lazy.force default

    | {bfip = (BFI.Loop, _) :: t; ir_l; depth; loops = true} ->
      let trec = prep_for_use trec in
      let trec = translate_rev
          {trec with bfip = t; ir_l = []; depth = depth + 1;} in
      let optloop = optimise_loop trec.ssize trec.ir_l in
      translate_rev {trec with ir_l = optloop @ ir_l}

    | {bfip = (BFI.Loopend, _) :: t; depth; loops = true} ->
      let trec = prep_for_use trec in
      {trec with bfip = t; depth = depth - 1;}

    (* Common operations *)
    | {bfip = (BFI.Incr, _) :: t; cond;} ->
      (let trec = {(prep_for_use trec) with bfip = t;} in
       let ir_l = trec.ir_l in
       let default =
         lazy (translate_rev {trec with ir_l = PI.Add 1 :: ir_l}) in
       if cond then match ir_l with
         | PI.Add 255 :: r ->
           translate_rev {trec with ir_l = r;}
         | PI.Push a :: r ->
           translate_rev {trec with ir_l = PI.Push (a + 1) :: r}
         | PI.Add a :: r ->
           translate_rev {trec with ir_l = PI.Add (a + 1) :: r}
         | _ -> Lazy.force default
       else Lazy.force default)

    | {bfip = (BFI.Decr, _) :: t; cond;} ->
      (let trec = prep_for_use trec in
       let ir_l = trec.ir_l in
       let default = lazy
         (translate_rev
            {trec with bfip = t; ir_l = PI.Subtract 1 :: ir_l}) in
       if cond then match ir_l with
         | PI.Subtract 255 :: r ->
           translate_rev {trec with bfip = t; ir_l = r;}
         | PI.Push a :: r ->
           translate_rev
             {trec with bfip = t; ir_l = PI.Push (a - 1) :: r}
         | PI.Subtract a :: r ->
           translate_rev
             {trec with bfip = t; ir_l = PI.Subtract (a + 1) :: r}
         | _ -> Lazy.force default
       else Lazy.force default)

    | {bfip = (BFI.Input, _)::t; cond; ir_l;} ->
      (let trec = prep_for_use trec in
       let ir_l = trec.ir_l in
       let default =
         lazy (translate_rev
                 {trec with bfip = t; ir_l = PI.Input :: ir_l;}) in
       if cond then match ir_l with
         | PI.Add _ :: r | PI.Subtract _ :: r ->
           translate_rev {trec with bfip = t; ir_l = PI.Input :: r;}
         | _ -> Lazy.force default
       else Lazy.force default)

    | {bfip = (BFI.Output, _) :: t; ir_l;} ->
      let trec = prep_for_use trec in
      translate_rev {trec with bfip = t; ir_l = PI.Output :: ir_l;}

    | {bfip = []; ir_l;} ->
      {trec with ir_l = PI.Eop :: ir_l;}

let rec rev_tree tr =
  let rec f acc = function
    | PI.Loop tr -> PI.Loop (rev_tree tr) :: acc
    | x -> x :: acc in
  List.fold_left f [] tr

(*
   FIXME: It seems that there is a logic bug here ...
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
  fun (l_subtree, r_subtree, middle, acc) ->
    match l_subtree, r_subtree, middle with

    | (lt :: l, rh :: r, []) ->
      shrink_rolls (lt :: l, r, [lt], acc)

    | PI.Roll (a, b) :: l, PI.Roll (c, d) :: r, _ ->
      if a = -c && b = d then
        let new_d = min d @@ abs c in
        (* No exception handling: last element is Roll (a,b) *)
        let middle_remove_last = middle |> List.(rev %> tl %> rev) in
        shrink_rolls (r, r, [],
                      PI.Roll (c, new_d)
                      :: middle_remove_last @ PI.Roll (a,new_d) :: acc)
      else
        shrink_rolls (PI.Roll (c,d) :: r, r, [PI.Roll (c,d)], middle @ acc)

    | PI.Roll (a, b) :: l, PI.Loop tr :: r, _ ->
      let (_, _, _, subtree) = shrink_rolls (tr, tr, [], []) in
      shrink_rolls (r, r, [], PI.Loop subtree :: PI.Roll (a, b) :: acc)

    | PI.Roll (a, b) :: l, rh :: r, _ ->
      shrink_rolls (PI.Roll (a,b) :: l, r, rh :: middle, acc)

    | PI.Roll (a,b) :: l, [], _ -> ([], [], [], middle @ acc)
    | lh :: l, _, _ -> shrink_rolls (l, l, [], middle @ acc)
    | [], _, _ -> ([], [], [], acc)

(* NOTE: Output is reversed. *)
let they_see_me_rollin_they_hatin ir_tree =
  let (_, _, _, small_tree) = shrink_rolls (ir_tree, ir_tree, [], []) in
  small_tree

(* NOTE: Input is supposed to be reversed order; output is in correct order. *)
let add_modulo_instr =
  let module Piet = Utils.Piet in
  let rec go acc tr =
    let rec f acc = function
      | PI.Loop tr -> PI.Mod 256 :: PI.Loop (go [PI.Mod 256] tr) :: acc
      | PI.Output -> PI.Mod 256 :: PI.Output :: acc
      | x -> begin
          match x with
          | PI.Input
          | PI.Push _
          | PI.Add _ | PI.Subtract _ | PI.Multiply _
          | PI.Op Piet.PAdd | PI.Op Piet.PSub | PI.Op Piet.PMul
            -> x :: PI.Mod 256 :: acc
          | _ -> x :: acc
        end in
    List.fold_left f [] tr in
  go []

let translate
    ?(condense = true) ?(loops_present = true)
    ?(stack_size = 8) ?(shrink_rolls = false)
    ?(modulo = false)
    bfip_l =
  let ir_l =
    if loops_present then
      List.make (stack_size - 1) (PI.Op Utils.Piet.PDup) @ [(PI.Push 0)]
    else [] in
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
  if shrink_rolls then
    if modulo then
      (add_modulo_instr % rev_tree @@ they_see_me_rollin_they_hatin trec.ir_l,
       trec.run_err)
    else (they_see_me_rollin_they_hatin trec.ir_l, trec.run_err)
  else
  if modulo then (add_modulo_instr trec.ir_l, trec.run_err)
  else (rev_tree trec.ir_l, trec.run_err)
