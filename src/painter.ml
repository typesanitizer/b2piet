open Batteries
open Utils.Piet

module V = BatVect
type 'a vec = 'a V.t
type picture = colour vec vec * int * int

type ir = Utils.PietIR.ir

module IRExpansion = struct
  module PI = Utils.PietIR

  type ir_draw = Grow of int | EOP [@@deriving show]
  type ir_mix = P of op | I of ir_draw | L of ir_mix list

  let rec show_ir_mix = function
    | L irml -> (Printf.sprintf "L: %s" @@ List.fold_lefti
                   (fun acc i b -> acc ^ (Printf.sprintf
                                            "    %s, %s\n"
                                            (string_of_int i)
                                            (show_ir_mix b))) "" irml)
    | I ird -> show_ir_draw ird
    | P o -> show_op o

  let expansion_func exp_f acc = function
    | PI.Push a -> exp_f acc (PI.Push a)
    | PI.Input  -> P PInpC :: acc
    | PI.Output -> P POutC :: P PDup :: acc
    | PI.Not    -> P PNot  :: acc
    | PI.Add a  -> P PAdd :: exp_f acc (PI.Push a)
    | PI.Subtract a -> P PSub :: exp_f acc (PI.Push a)
    | PI.Multiply a -> P PMul :: exp_f acc (PI.Push a)
    | PI.Mod a      -> P PMod :: exp_f acc (PI.Push a)
    | PI.Roll (a,b) -> P PRoll :: exp_f (exp_f acc (PI.Push b)) (PI.Push a)
    | PI.Loop ir_l  -> L (List.rev @@ List.fold_left exp_f [] ir_l) :: acc
    | PI.Eop        -> I EOP :: acc
    | PI.Op op      -> P op :: acc

  module Naive = struct

    let expand =
      let rec f acc = function
        | PI.Push a ->
          (match compare a 0 with
           | x when x > 0 -> P PPush :: I (Grow a) :: acc
           | 0 -> P PNot :: f acc (PI.Push 1)
           | _ -> P PSub :: f (f acc (PI.Push 1)) (PI.Push (-a + 1)))
        | y -> expansion_func f acc y in
      List.rev % List.fold_left f []
  end

  module Fast = struct

    module FP = Utils.FastPush

    let push_op_to_irml = function
      | FP.Number x -> [P Utils.Piet.PPush; I (Grow x);]
      | FP.PDup     -> [P Utils.Piet.PDup;]
      | FP.Binary FP.PAdd -> [P Utils.Piet.PAdd;]
      | FP.Binary FP.PSub -> [P Utils.Piet.PSub;]
      | FP.Binary FP.PMul -> [P Utils.Piet.PMul;]
      | FP.Binary FP.PDiv -> [P Utils.Piet.PDiv;]
      | FP.Binary FP.PMod -> [P Utils.Piet.PMod;]

    let expand fpl =
      let fastpush_table =
        fpl
        |> List.map (fun (_, c, l) ->
            (c, List.fold_left (fun a x -> (push_op_to_irml x) @ a) [] l))
        (* prepend entry for 0, now list index = number to be pushed *)
        |> List.cons (2, [P Utils.Piet.PNot; P Utils.Piet.PPush;])
        |> Array.of_list
      in
      let rec f acc = function
        | PI.Push a ->
          if a >= 0 then (snd @@
                          try Array.get fastpush_table a with
                            Invalid_argument s ->
                            let _ = print_endline @@ Printf.sprintf
                                "OOB access fpt %d" a in
                            exit 1
                         ) @ acc
          else (P PSub) :: f (f acc (PI.Push 1)) (PI.Push (- a + 1))
        | y -> expansion_func f acc y in
      List.rev % List.fold_left f []
  end

  let interpret ir_l =
    let rec interpret_main (n1, foo) bar =
      let maphead f v =
        let (h, t) = V.shift v in
        V.prepend (f h) t in
      let maphead2 f v =
        let (a, t) = V.shift v in
        let (b, t) = V.shift t in
        V.prepend (f b a) t in
      let f (n,stack) z =
        match z with
        | PI.Push a -> (n + 1, V.prepend a stack)
        | PI.Input  -> raise (Failure "input unexpected.")
        | PI.Output ->
          (try
             let _ = print_char % char_of_int % V.first @@ stack in
             let _ = flush stdout in
             (n, stack)
           with
             Invalid_argument s ->
             print_endline @@ Printf.sprintf "intc -> %d" (V.first stack);
             raise (Invalid_argument s))
        | PI.Not    -> (n, maphead (fun x -> if x = 0 then 1 else 0) stack)
        | PI.Add a  -> (n, maphead ((+) a) stack)
        | PI.Subtract a -> (n, maphead (fun x -> x - a) stack)
        | PI.Multiply a -> (n, maphead (( * ) a) stack)
        | PI.Mod a      -> (n, maphead (fun x -> (a + x) mod a) stack)
        | PI.Roll (a, b) ->
          let rec go a b c = match a with
            | x when x > 0 ->
              let (h, t) = V.shift c in
              if b <> V.length c then
                (* NOTE: the b - 1 below should actually be b - 2 according to
                   batteries documentation but there is an off-by-one error in
                   library.*)
                go (a - 1) b V.(insert (b - 1) (singleton h) t)
              else go (x - 1) b (V.append h t)
            | x when x < 0 ->
              let y = V.get c (b - 1) in
              let z = V.remove (b - 1) 1 c in
              go (a + 1) b (V.prepend y z)
            | _ -> c in
          (n - a, go a b stack)
        | PI.Loop ir_l  ->
          let inner = V.of_list ir_l in
          let rec go (n2, acc) = match V.first acc with
            | 0 -> (n2, acc)
            | _ -> go (interpret_main (n2, acc) inner) in
          go (n, stack)
        | PI.Eop        -> (n, stack)
        | PI.Op PDup    -> (n + 1, V.prepend (V.first stack) stack)
        | PI.Op PSub    -> (n - 1, maphead2 (-) stack)
        | PI.Op PAdd -> (n - 1, maphead2 (+) stack)
        | _ -> raise (Failure "unexpected operation")
      in
      V.fold_left f (n1, foo) bar
    in
    interpret_main (-288, V.empty) @@ V.of_list ir_l |> snd

end


module CanonicalDraw = struct

  module IR = Utils.PietIR
  module IRE = IRExpansion

  (* NOTE:
     Use of Naive is restricted to cases when both Naive and Fast overlap. *)
  module IREN = IRExpansion.Naive

  type ir_block = xy * bool * picture

  type ir_draw = DrawWhite | Random | Cp of int | Grow of int | Eop
  [@@deriving show]
  type ir_mix = P of op | I of ir_draw | L of ir_mix list

  let rec upcast = function
    | IRE.P op -> P op
    | IRE.I ir -> (match ir with
        | IRE.Grow a -> I (Grow a)
        | IRE.EOP  -> I (Eop))
    | IRE.L irl -> L (List.map upcast irl)

  let rec fill_rows cur_y (clr_a, w, h) =
    if cur_y = h then (clr_a, w, h)
    else
      let cur_row = V.get clr_a cur_y in
      let black_row = V.make (w - V.length cur_row) Black in
      fill_rows (cur_y + 1)
        (V.modify clr_a cur_y (fun r -> V.concat r black_row), w, h)

  let fill_below = fill_rows 1

  let rec draw_linear_right :
    int * colour * int * picture -> ir_mix -> int * colour * int * picture =

    fun (depth, cur_clr, sz, (p_clr_a2d, w, h)) ->

      let append_1 clr =
        fill_below (V.modify p_clr_a2d 0 (V.append clr), w + 1, h) in
      let concat_1d dw c_clr_a1d =
        fill_below
          (V.modify p_clr_a2d 0 (fun z -> V.concat z c_clr_a1d), w + dw, h) in
      let rec concat_2d (p_clr_a2d, w, h) (clr_a2d, dw, new_h) =
        if new_h <= h then
          let p_clr_a2d =
            V.foldi (fun i acc row -> V.modify acc i (fun z -> V.concat z row))
              p_clr_a2d clr_a2d in
          fill_rows new_h (p_clr_a2d, w + dw, h)
        else
          let new_p = V.concat p_clr_a2d @@ V.make (new_h - h) V.empty in
          let new_p = fill_rows h (new_p, w, new_h) in
          concat_2d new_p (clr_a2d, dw, new_h) in

      function
      | P op ->
        if cur_clr = White then
          let rand_clr = Green (* 18 |> Random.int |> num_to_colour *) in
          let next_clr = op_next_colour op rand_clr in
          let tl = V.singleton rand_clr |> V.append next_clr in
          (depth, next_clr, 1, concat_1d 2 tl)
        else
          let next_clr = op_next_colour op cur_clr in
          (depth, next_clr, 1, append_1 next_clr)

      | I DrawWhite  ->
        if cur_clr = White then (depth, Blue, 1, append_1 Blue)
        else (depth, White, 1, append_1 White)
      | I Random ->
        let rand_clr = Red (*18 |> Random.int |> num_to_colour*) in
        (depth, rand_clr, 1, append_1 rand_clr)

      (* assert a > 0 *)
      | I Cp a ->
        (depth, cur_clr, a + 1, cur_clr |> V.make a |> concat_1d a)

      (* assert a > 0 *)
      | I Grow a  ->
        (if cur_clr = White then
           let rand_clr = Blue in
           draw_linear_right
             (depth, rand_clr, 1, append_1 rand_clr) (I (Grow a))
         else
           let acc = (depth, cur_clr, sz, (p_clr_a2d, w, h)) in
           match compare sz a with
           | x when x > 0  -> List.fold_left
                                draw_linear_right
                                acc [I DrawWhite; I Random; I (Cp (a-1))]
           | x when x < 0 -> draw_linear_right acc (I (Cp (a-sz)))
           | _  -> acc)

      | I Eop ->
        let temp =
          V.(of_list [
              of_list [White; White; Green; Black;  Cyan;];
              of_list [Black; Black; White; Black;  Cyan;];
              of_list [Black; Green; Green; Green; Black;];
              of_list [Black; Black; Black; Black;  Cyan;];
            ]) in
        (depth, White, 1, concat_2d (p_clr_a2d, w, h) (temp, 5, 4))

      | L (irm_l) ->
        let (_, _, _, (c_clr_a, c_w, c_h)) =
          (* let rand_clr = 18 |> Random.int |> num_to_colour in *)
          List.fold_left
            draw_linear_right
            (depth+1, White, 0, (V.singleton V.empty, 0, 1))
            irm_l in

        let make_vec l =
          let (_, _, _, (arr2d, tv_w, _)) =
            List.fold_left draw_linear_right
              (depth, White, 0, (V.singleton V.empty, 0, 1)) l in
          (V.get arr2d 0, tv_w) in

        let (turn_col, tc_h) =
          if depth mod 2 = 0 then
            (V.singleton White |> V.append Green, 2)
          else
            let (ac_turn_col, ac_turn_col_len) =
              (IREN.expand [IR.Push 3]) @ [IRE.P PPtr]
              |> List.map upcast |> make_vec in
            (V.concat (V.singleton White) ac_turn_col, ac_turn_col_len + 1)
        in

        (* black border of 1 codel on each side *)
        let boundary_w = 1 in

        (* +2 is due to white codels needed between cell and entry/exit *)
        let extra_w = 2 * boundary_w + tc_h + 2 in

        let entry_x = c_w + extra_w - 1 - boundary_w in
        let exit_x = boundary_w in

        (* Construct "container" for child *)
        let concat_below i acc = fun row ->
          V.(backwards row
             |> of_enum
             |> (fun z -> if i = 0 then
                    z |> concat (of_enum @@ backwards turn_col)
                    |> append White |> append (last turn_col)
                  else
                    z |> concat (make tc_h Black)
                    |> append Black |> append Black)
             |> prepend Black
             |> append Black
             |> make 1
             |> concat acc) in

        let set n c v = V.set v n c in

        (* child overlaps with tc_h at one point, and there are two layers
           over entry turn_col, top -> PPtr and second -> White *)
        let extra_h = tc_h + 1 in

        (* vector for entry into loop and starting coordinate *)
        let (loop_vec, lv_w) =
          if depth mod 2 = 0 then
            make_vec [P PDup; P PNot; P PNot; P PPtr]
          else
            [IRE.P PDup; IRE.P PNot; IRE.P PNot;]
            @ (IREN.expand [IR.Multiply 3]) @ [IRE.P PPtr]
            |> List.map upcast |> make_vec in
        let start = (entry_x - (lv_w - 1)) in
        let apply_turn top_row =
          V.foldi (fun i a c -> set (i + start) c a) top_row loop_vec in

        let top_row = V.make (c_w + extra_w) White
                      |> set exit_x (V.last turn_col)
                      |> apply_turn in
        let turning_help_rows =
          V.pop turn_col |> snd
          |> fun tc_pop -> V.mapi
            (fun i c ->
               V.make (c_w + extra_w) Black
               |> set entry_x c
               |> set exit_x (V.get tc_pop (tc_h - 2 - i)))
            tc_pop in
        let new_arr2d = turning_help_rows
                        |> V.concat (V.make 1 top_row)
                        |> fun z -> (V.foldi concat_below z c_clr_a) in

        (depth, White, 1,
         concat_2d (p_clr_a2d, w, h) (new_arr2d, c_w + extra_w, c_h + extra_h))

  let draw_linear irm_l =
    let (_, _, _, full_pic) =
      List.fold_left
        (fun a b -> draw_linear_right a (upcast b))
        (0, White, 0, (V.singleton V.empty, 0, 1))
        irm_l in
    full_pic
end

(*
   X.num_ops indicates the number of transitions allowed inside a rule for
   the tableau. So it is be equal to the desired `rule_width - 1` where
   rule_width is in codel units.
   NOTE: X.num_ops >= 3 is required, otherwise anti-clockwise turns will break.
   It is okay to use 1 <= X.num_ops <= 2 when only clockwise turns are needed
   but thin rules may look ugly.
*)
module Mondrian(X : sig val num_ops : int end) = struct

  module TableauLayout = struct

    (* The turn structures have transitions = X.num_ops + 1 where the +1 is to
       redirect the flow with the pointer instruction. *)
    let cw_turn =
      V.make (X.num_ops - 1) PNop
      |> V.prepend @@ Utils.Piet.PPush
      |> V.append @@ Utils.Piet.PPtr
    let acw_turn =
      V.make 2 PNop
      |> V.append Utils.Piet.PPush
      |> fun z -> V.concat z (V.make (X.num_ops - 3) PNop)
                  |> V.append Utils.Piet.PPtr

    module IRE = IRExpansion

    type linear_layout = Segment of op V.t
                       | LinLoop of linear_layout V.t
                       | LinEOP

    (* NOTE: The last argument to make_straight has items of variants IRE.P
       and IRE.I Grow only. Furthermore, every Grow is preceded by a PPush. *)
    let rec make_straight acc_v rem cur_v = function
      | x when rem = 0 ->
        make_straight (V.prepend cur_v acc_v) X.num_ops V.empty x

      | IRE.P PPush :: IRE.I (IRE.Grow n) :: t when rem >= n ->
        let cur_v = V.(concat (make (n - 1) PNop |> append PPush) cur_v) in
        make_straight acc_v (rem - n) cur_v t

      | IRE.P PPush :: IRE.I (IRE.Grow n) :: t when rem >= 2 ->
        let cur_v =
          V.(concat (make (rem - 2) PNop |> append PPush |> append PAdd) cur_v)
        in
        make_straight (V.prepend cur_v acc_v) X.num_ops V.empty
          IRE.(P PPush :: I (Grow (n - rem + 1)) :: t)

      (* NOTE: rem = 1 for this branch so no space for operations *)
      | IRE.P PPush :: IRE.I (IRE.Grow n) :: t ->
        let cur_v = V.append PNop cur_v in
        make_straight (V.prepend cur_v acc_v) X.num_ops V.empty
          IRE.(P PPush :: I (Grow n) :: t)

      | IRE.P op :: t ->
        make_straight acc_v (rem - 1) (V.prepend op cur_v) t
      | [] ->
        let rem = (if X.num_ops = rem then 0 else rem) in
        V.prepend (V.concat cur_v (V.make rem PNop)) acc_v
      | IRE.I (IRE.Grow n) :: t ->
        raise (Failure "make_straight received Grow not preceded by PPush.")
      | _ -> raise (Failure "Unexpected arguments for make_straight.")

    (*
       Creates a partial layout using IR. The straight sections are split
       up into vectors of length exactly X.num_ops, so that they can be overlaid
       onto (vertical) rules directly.
    *)
    let to_linlayout =
      let rec go dep irml =
        let rec f (dep, stack, acc) =
          let prev_straight_v =
            lazy (if stack = [] then V.empty
                  else V.map (fun x -> Segment x)
                      (make_straight V.empty X.num_ops V.empty stack)) in
          function
          | (IRE.L irm_l) :: xs ->
            let inner =  LinLoop (go (dep + 1) @@
                                  IRE.(P PPush :: P PPop :: irm_l)) in
            let acc = V.concat acc (Lazy.force prev_straight_v)
                      |> V.append inner in
            f (dep, [], acc) xs
          | (IRE.I IRE.EOP) :: xs ->
            let acc = V.concat acc (Lazy.force prev_straight_v)
                      |> V.append LinEOP in
            f (dep, [], acc) xs
          | x :: xs -> f (dep, x :: stack, acc) xs
          | [] -> (dep, [], V.concat acc (Lazy.force prev_straight_v)) in
        let (_, _, acc) = f (dep, [], V.empty) irml in
        acc in
      go 0

    type chunk = Stick of op V.t
               | Fence of info
               | ChunkEOP and
    chunkblock = Row of chunk V.t
               | Turn of int
               | LoopTop and
    info = {
      width : int;
      tot_h : int;
      row_h : int list;
      n_row : int;
      blank : int list;
      cost  : float;
      inner : chunkblock list;
    }

    let rec show_info i =
      let show_l f = List.fold_left
          (fun a x -> f x |> (^) a |> flip (^) ";") "" in
      let show_v f = V.fold_left
          (fun a x -> f x |> (^) a |> flip (^) "\n\t,") "" in
      let show_cb =
        let show_chunk = function
          | Stick opv -> "Stick\n\t" ^ show_v show_op opv ^ "StickEnd\n"
          | ChunkEOP -> "ChunkEOP"
          | Fence info -> "Fence\n" ^ show_info info ^ "FenceEnd\n" in
        function
        | Row cv -> show_v show_chunk cv
        | Turn n -> Printf.sprintf "Turn %d\n" n
        | LoopTop -> "LoopTop"
      in
      Printf.sprintf "width : %d \n\
                      tot_h : %d \n\
                      row_h : %s \n\
                      n_row : %d \n\
                      blank : %s \n\
                      cost  : %f \n\
                      inner : %s \n"
        i.width i.tot_h (show_l string_of_int i.row_h) i.n_row
        (show_l string_of_int i.blank) i.cost (show_l show_cb i.inner)

    let meta_chunk = function
      | Stick v -> (1, 1, Stick v)
      | Fence i -> (i.width, i.tot_h, Fence i)
      | ChunkEOP -> (1, 1, ChunkEOP)

    let is_fence = function
      | (_, _, Fence _) -> true
      | _ -> false

    let empty_info = {
      width = 0;
      tot_h = 0;
      row_h = [];
      n_row = 1;
      blank = [];
      cost  = 0.;
      inner = [];
    }

    let wh_cost phi w h =
      let r = float h /. float w in
      let a = float w *. float h in
      a *. (abs_float @@ (if r >= 1. then r else 1./.r) -. phi)**2.0

    let rec info_of_n prog_meta phi n tot_w =
      let fill n (tot_w, pmeta, info) =
        let init_rem k =
          if n = 1 then tot_w
          else if k = 1 || k = n then tot_w - 1
          else tot_w - 2
        in
        let rec f info = function
          | ([], 1, 0) -> (false, {
              info with cost = info.cost +. wh_cost phi tot_w info.tot_h;
            })
          | (_, 1, 0) -> (true, {
              info with cost = info.cost +. wh_cost phi tot_w info.tot_h;
            })

          | ([], n, r) ->
            let row_h = info.row_h in
            let inner = Row V.empty :: Turn (List.hd row_h) :: info.inner in
            let inner =
              let rec g acc = function
                | 0 -> acc
                | k -> g (Row V.empty :: Turn 1 :: acc) (k - 1) in
              g inner (n - 1) in
            let tot_h = info.tot_h + n in
            let info = {
              info with tot_h;
                        row_h = List.make n 1 @ row_h;
                        n_row = info.n_row + n;
                        blank = (tot_w - 1) :: List.make (n - 1) (tot_w - 2)
                                @ r :: info.blank;
                        cost = info.cost +. wh_cost phi tot_w tot_h;
                        inner;
            } in
            (false, info)

          | ((w, h, dc) :: t, n, r) when w > r ->
            let _ = if n < -2 then exit 0 in
            if n = 1 || w > tot_w - 1 || (t <> [] && w > tot_w - 2) then
              (* beta, tumse na (fit) ho payega https://youtu.be/biqHU4BKuLc
                 "Son, you won't be able to do it (fitting the chunk inside)" *)
              (true, info)
            else
              let (turn_h, row_h) = match info.row_h with
                | -1 :: xs -> (1, -1 :: 1 :: xs)
                | x :: xs -> (x, -1 :: x :: xs)
                | [] -> raise (Failure "Case should've been caught earlier")
              in
              ((w, h, dc) :: t, n - 1, init_rem (n - 1)) |> f {
                info with tot_h = info.tot_h + 1;
                          row_h;
                          n_row = 1 + info.n_row;
                          blank = r :: info.blank;
                          inner = Turn turn_h :: info.inner;
              }

          | ((w, h, ch) :: t, n, r) ->
            let (tot_h, row_h) = match info.row_h with
              | x :: xs ->
                let cur_h = max h x in
                (info.tot_h + cur_h - x, (max h x) :: xs)
              | [] -> (info.tot_h + h, [h])
            in
            let cost = info.cost +. match ch with
              | Fence f_info -> f_info.cost
              | _ -> 0. in
            let inner = match info.inner with
              | Row ch_v :: tl -> Row (V.append ch ch_v) :: tl
              | tl -> Row (V.singleton ch) :: tl in
            f {info with tot_h; row_h; cost; inner;} (t, n, r - w)
        in
        f info (pmeta, n, init_rem n)
      in
      let (leftover, info) =
        fill n (tot_w, V.to_list prog_meta, {empty_info with width = tot_w;}) in
      if leftover then
        info_of_n prog_meta phi n (tot_w + 1)
      else
        {info with cost = info.cost +. wh_cost phi info.width info.tot_h}

    (*
       IMHO, the placement logic is slightly complicated.

       Example:
       If tmp.tot_h = 1, the inner part will not share any vertical rules
       with the parent loop, so 2 columns are required. Otherwise, one
       vertical rule is shared, so only 1 additional column is required.

       To understand the use of "magic numbers" / if-else values for variables,
       see `../layout.org`.
    *)
    let rec placement phi progv =
      let f phi = function
        | Segment op_v -> Stick op_v
        | LinEOP -> ChunkEOP
        | LinLoop pv ->
          let phi = max 1. (phi -. 0.2) in
          let tmp = placement phi pv in
          let extra_h = 1 in
          let extra_w = if tmp.tot_h = 1 then 2 else 1 in
          let min_w = 3 in
          let outer_w = max min_w (tmp.width + extra_w) in
          let tot_h = tmp.tot_h + extra_h in
          Fence {
            width = outer_w;
            tot_h;
            row_h = 1 :: tmp.row_h;
            n_row = tmp.n_row + 1;
            blank = (outer_w - 3) :: tmp.blank;
            cost  = tmp.cost +. wh_cost phi outer_w tot_h;
            inner = LoopTop :: tmp.inner;
          }
      in
      let prog_meta = V.map (meta_chunk % (f phi)) progv in
      let is_tall = V.exists (fun (_, h, _) -> h > 1) prog_meta in

      let inject_w f a (w, _, _) = f a w in
      let tot_len = V.fold_left (inject_w (+)) 0 prog_meta in
      let max_inner_w = V.filter is_fence prog_meta
                        |> V.fold_left (inject_w max) 1 in

      (* TODO: think about weird edge case -- tot_len <= 4 && is_tall *)
      if tot_len <= 4 && (not is_tall) then {
        width = tot_len;
        tot_h = 1;
        row_h = [1];
        n_row = 1;
        blank = [0];
        cost  = wh_cost phi tot_len 1;
        inner = [Row (V.map (fun (_, _, x) -> x) prog_meta)];
      }
      else
        let min_outer_w = max_inner_w + (if is_tall then 1 else 2) in
        let rec go n =
          let info = info_of_n prog_meta phi n min_outer_w in
          function
          | Some best when best.cost <= info.cost ->
            {
              best with row_h = List.rev best.row_h;
                        blank = List.rev best.blank;
                        inner = List.rev best.inner;
            }
          | _ -> go (n + 2) (Some info)
        in
        go 1 None

    let ir_mix_list_to_info = placement Utils.golden_ratio % to_linlayout

  end

  module TableauPaint = struct

    let primes = BatSet.of_list [
        2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53;
        59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109;
        113; 127; 131; 137; 139; 149; 151; 157; 163; 167; 173;
        179; 181; 191; 193; 197; 199; 211; 223; 227; 229; 233; 239;
        241; 251;
      ]

    let rec factor_pairs n =
      if n < 14 then []
      else if BatSet.mem n primes then
        let s = factor_pairs (n-1) in
        if s = [] then []
        else s
      else
        let phi = Utils.golden_ratio in
        let n_f = float n in
        let golden_d = int_of_float @@ sqrt (n_f /. phi) in
        let delta_d = max 1 @@ int_of_float (sqrt n_f -. sqrt (n_f /. phi)) in
        let l1 = List.fold_left
            (fun l d -> if n mod d = 0 then (d, n/d)::l else l)
            [] @@ List.range golden_d `To (golden_d + delta_d) in
        let l2 = List.fold_left
            (fun l d -> if n mod d = 0 then (d, n/d)::l else l)
            [] @@ List.range (golden_d - delta_d) `To (golden_d)
        in
        (l1 @ l2)
        |> List.sort_uniq
          (fun (d1,d2) (d3,d4) ->
             compare
               (abs_float ((float d2)/.(float d1)) -. phi)
               (abs_float ((float d4)/.(float d3)) -. phi))
        |> List.filter (fun (d1, d2) -> (float d2)/.(float d1) < 21./.9.)

    type ruledir = V | H
    type rule = {
      dir       : ruledir;
      top_left  : xy;
      bot_right : xy;
      nonblack  : codel list;
    }

    let stick_rule x y w h rel_codels =
      let abs_codels = List.map
          (fun (c, x', y') -> (c, x + x', y + y')) rel_codels in
      {
        dir = V;
        top_left = (min x (x + w - 1), min y (y + h - 1));
        bot_right = (max x (x + w - 1), max y (y + h - 1));
        nonblack = abs_codels;
      }

    type turn = CW | ACW
    let reverse = function
      | CW -> ACW
      | ACW -> CW
    let sign = function
      | CW -> 1
      | ACW -> -1

    let codels_of_opv turn =
      let startc = DarkMagenta in
      V.foldi (fun i a op -> let (c, _, _) = List.hd a in
                (op_next_colour op c, (i + 1) * sign turn, 0) :: a) [(startc, 0, 0)]

    let thickness l = match l.dir with
      | V -> (fst l.bot_right) - (fst l.top_left)
      | H -> (snd l.bot_right) - (snd l.top_left)
    let length l = match l.dir with
      | V -> (snd l.bot_right) - (snd l.top_left)
      | H -> (fst l.bot_right) - (fst l.top_left)

    type panel = {
      fill      : colour;
      extra     : codel list;
      enter     : xy;
      leave     : xy;
      top_left  : xy;
      bot_right : xy;
      flow      : xy list;
    }

    let whitepanel x y w h = {
      fill = White;
      extra = [];
      enter = (x, y);
      leave = (x + w - 1, y + h - 1);
      top_left = (min x (x + w - 1), min y (y + h - 1));
      bot_right = (max x (x + w - 1), max y (y + h - 1));
      flow = [(x, y); (x + w - 1, y)];
    }

    type element = Panel of panel | Rule of rule

    let top_left = function
      | Panel p -> p.top_left
      | Rule l -> l.top_left
    let bot_right = function
      | Panel p -> p.bot_right
      | Rule l -> l.bot_right
    let extra = function
      | Panel p -> p.extra
      | Rule l -> l.nonblack
    let fill = function
      | Panel p -> p.fill
      | Rule l -> Black

    let cw_ptr_clr = LightCyan
    let acw_ptr_clr = LightYellow
    (* let cw_ptr_clr = LightCyan in *)
    (* let cw_ptr_prev_clr = op_prev_colour PPtr cw_ptr_clr in *)
    (* (V.make X.num_ops cw_ptr_prev_clr *)
    (*  |> V.prepend @@ op_prev_colour PPush cw_ptr_prev_clr *)
    (*  |> V.append cw_ptr_clr, *)

    (* let append_vec a1 a2 = V.concat a2 a1 in *)
    (* let acw_ptr_clr = LightYellow in *)
    (* let acw_ptr_prev_clr = op_prev_colour PPtr acw_ptr_clr in *)
    (* (V.make 3 @@ op_prev_colour PPush acw_ptr_prev_clr *)
    (*  |> append_vec @@ V.make (X.num_ops - 2) acw_ptr_prev_clr *)
    (*  |> V.append acw_ptr_prev_clr, *)

    type paint_state = {
      flow : turn;
    }

    module ST = Utils.SplayTree(
      struct
        type t = element
        type s = xy
        let t_compare e1 e2 =
          let  (xy1, xy2) = (top_left e1, top_left e2) in
          compare_xy xy1 xy2
        let s_inside_t pt e =
          let (xy1, xy2) = (top_left e, bot_right e) in
          match compare_xy pt xy1 with
          | Utils.LT -> Utils.LT
          | _ -> (match compare_xy pt xy2 with
              | Utils.GT -> Utils.GT
              | _ -> Utils.EQ)
      end)

    let panel_to_rule_size_ratio = 6.0
    let rule_w = X.num_ops + 1

    (* Generates a low-discrepancy sequence of locations (i.e. roughly evenly
       spaced). See the section titled "Additive recurrence":
       https://en.wikipedia.org/wiki/Low-discrepancy_sequence. *)
    let rule_locs n init =
      let phi = Utils.golden_ratio -. 1.0 in
      let rec f acc x = function
        | 0 -> acc
        | n ->
          let next = x +. phi in
          let next = if next < 1.0 then next else next -. 1.0 in
          f (x :: acc) next (n - 1) in
      let a = List.sort compare (f [] init n) in
      let rec min_diff m = function
        | x :: x' :: xs -> min_diff (min m (x' -. x)) (x' :: xs)
        | [x] -> min m (1.0 -. x)
        | _ -> raise (Failure "Unreachable.") in
      let min_d = min_diff (1.0 -. List.hd a) a in
      let scaled_rw = min_d /. panel_to_rule_size_ratio in
      let scale f = (f /. scaled_rw) *. float rule_w in
      let shift f = f -. (scaled_rw /. 2.0) in
      let affine = shift %> scale %> int_of_float in
      let w = int_of_float (scale 1.0) in
      (w, List.map affine a)

    let try_init = List.(map (fun x -> float x /. 20.0) @@ range 1 `To 19)

    let good_rule_locs norm n =
      List.map (rule_locs n) try_init
      |> List.min_max ~cmp:(fun (l, _) (l', _) -> compare (norm l) (norm l'))
      |> fst
    let good_vrule_locs = good_rule_locs (fun x -> x)
    let good_hrule_locs best_h = (fun h -> abs (h - best_h))

    type composition = ST.t * int * int

    let composition_of_info :
      paint_state -> TableauLayout.info -> composition =
      let module Tbl = TableauLayout in
      fun ps info -> Tbl.(ST.empty, info.width, info.tot_h)

    let draw_picture : composition -> picture = fun (elem_st, w, h) ->
      let set2d x y c a = V.modify a y (fun row -> V.set row x c) in
      let elem_vec = ST.to_vec elem_st in
      let pic_arr =
        V.make h (V.make w Black)
        |> fun init ->
        V.fold_left
          (fun array e ->
             let ((x1, y1), (x2, y2), cl) = (top_left e, bot_right e, extra e) in
             List.fold_left (fun arr y ->
                 List.fold_left (fun ar x -> set2d x y (fill e) ar)
                   arr (List.range x1 `To x2))
               array (List.range y1 `To y2)
             |> fun filled ->
             List.fold_left (fun arr (c, cx, cy) -> set2d cx cy c arr) filled cl
          )
          init elem_vec
      in
      (pic_arr, w, h)
  end

end

module M = Mondrian(struct let num_ops = 5 end)

let tableau_show fpl = M.TableauLayout.(show_info % ir_mix_list_to_info)
                       % IRExpansion.Fast.expand fpl

(* let printcv = print_endline % V.foldi (fun i a b -> *)
(*     a ^ (Printf.sprintf "%d, %s \n" i @@ a ^ (show_colour b))) "" *)

(* let print_irm = print_endline % List.fold_lefti (fun a i b -> *)
(*     a ^ (Printf.sprintf "%d, %s \n" i @@ NaiveIRExpansion.show_ir_mix b)) "" *)

(* let paint_linear i = *)
(*   let x = NaiveIRExpansion.expand i in *)
(*   let _ = print_irm x in *)
(*   CanonicalDraw.draw_linear x *)

type push_style = Literal
                | Fast of (int * int * Utils.FastPush.push_op list) list
type draw_style = Linear | Tableau

let paint ps ds =
  let expansionfn = IRExpansion.(match ps with
      | Literal -> Naive.expand
      | Fast fpl -> Fast.expand fpl) in
  let drawfn = match ds with
    | Linear -> CanonicalDraw.draw_linear
    | Tableau -> CanonicalDraw.draw_linear in
  drawfn % expansionfn

let interpret = IRExpansion.interpret
