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
    | PI.Dup        -> P PDup :: acc
    | PI.Eop        -> I EOP :: acc

  module Naive = struct

    let expand =
      let rec f acc = function
        | PI.Push a ->
          (match compare a 0 with
           | x when x > 0 -> P PPush :: I (Grow a) :: acc
           | 0 -> P PNot :: f acc (PI.Push 1)
           | _ -> P PSub :: f (f acc (PI.Push 1)) (PI.Push (-a+1)))
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
      | FP.Binary FP.PMod -> [P Utils.Piet.PAdd;]

    let expand ssize =
      let max_opsize = max 256 ssize in
      let fastpush_table =
        FP.fast_push 5 max_opsize
        |> List.map (fun (_, c, l) ->
            (c, List.fold_left (fun a x -> (push_op_to_irml x) @ a) [] l))
        |> V.of_list
        |> V.concat @@ V.singleton (2, [P Utils.Piet.PNot; P Utils.Piet.PPush;])
      in
      let rec f acc = function
        | PI.Push a ->
          if a >= 0 then (snd @@ V.get fastpush_table a) @ acc
          else (P PSub) :: f (f acc (PI.Push 1)) (PI.Push (-a+1))
        | y -> expansion_func f acc y in
      List.rev % List.fold_left f []
  end

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

  let rec fill_rows : int -> picture -> picture = fun cur_y (clr_a, w, h) ->
    if cur_y = h then (clr_a, w, h)
    else
      let cur_row = V.get clr_a cur_y in
      let black_row = V.make (w - V.length cur_row) Black in
      fill_rows (cur_y+1)
        (V.modify clr_a cur_y (fun r -> V.concat r black_row), w, h)

  let fill_below = fill_rows 1

  let rec draw_linear_right :
    int * colour * int * picture -> ir_mix -> int * colour * int * picture =

    fun (depth, cur_clr, sz, (p_clr_a2d, w, h)) ->

      let append_1 clr =
        fill_below (V.modify p_clr_a2d 0 (V.append clr), w+1, h) in
      let concat_1d dw c_clr_a1d =
        fill_below
          (V.modify p_clr_a2d 0 (fun z -> V.concat z c_clr_a1d), w + dw, h) in
      let rec concat_2d (p_clr_a2d, w, h) (clr_a2d, dw, new_h) =
        if new_h <= h then
          fill_rows new_h
            (V.foldi (fun i acc row -> V.modify acc i (fun z -> V.concat z row))
               p_clr_a2d clr_a2d, w + dw, h)
        else let new_p = fill_rows h
                 (V.concat p_clr_a2d @@ V.make (new_h-h) V.empty, w, new_h) in
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
        (depth, cur_clr, a+1, cur_clr |> V.make a |> concat_1d a)

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
          V.of_list [
            V.of_list [White; White; Green; Black;  Cyan;];
            V.of_list [Black; Black; White; Black;  Cyan;];
            V.of_list [Black; Green; Green; Green; Black;];
            V.of_list [Black; Black; Black; Black;  Cyan;];
          ] in
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
          V.backwards row
          |> V.of_enum
          |> (fun z -> if i = 0 then
                 z |> V.concat (V.of_enum @@ V.backwards turn_col)
                 |> V.append White |> V.append (V.last turn_col)
               else
                 z |> V.concat (V.make tc_h Black)
                 |> V.append Black |> V.append Black)
          |> V.prepend Black
          |> V.append Black
          |> V.make 1
          |> V.concat acc in

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

    type ruledir = V | H
    type rule = {
      dir       : ruledir;
      top_left  : xy;
      bot_right : xy;
      nonblack  : codel list;
    }
    let thickness l = match l.dir with
      | V -> (fst l.bot_right) - (fst l.top_left)
      | H -> (snd l.bot_right) - (snd l.top_left)
    let length l = match l.dir with
      | V -> (snd l.bot_right) - (snd l.top_left)
      | H -> (fst l.bot_right) - (fst l.top_left)
    type panel = {
      fill      : colour;
      extra     : codel list;
      entry     : xy;
      exit      : xy;
      top_left  : xy;
      bot_right : xy;
      flow      : xy list;
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

    type composition = ST.t * int * int

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
        let cur_v = V.concat (V.make (n - 1) PNop) cur_v |> V.append PPush in
        make_straight acc_v (rem - n) cur_v t
      | IRE.P PPush :: IRE.I (IRE.Grow n) :: t when rem >= 2 ->
        let cur_v = V.concat (V.make (rem - 2) PNop
                              |> V.append PPush
                              |> V.append PAdd) cur_v
        in
        make_straight (V.prepend cur_v acc_v) X.num_ops V.empty
          (IRE.P PPush :: IRE.I (IRE.Grow (n - (rem - 1))) :: t)
      (* NOTE: rem = 1 for this branch so no space for operations *)
      | IRE.P PPush :: IRE.I (IRE.Grow n) :: t ->
        let cur_v = V.append PNop cur_v in
        make_straight (V.prepend cur_v acc_v) X.num_ops V.empty
          (IRE.P PPush :: IRE.I (IRE.Grow n) :: t)

      | IRE.P op :: t ->
        make_straight acc_v (rem - 1) (V.prepend op cur_v) t
      | [] -> V.prepend (V.concat cur_v (V.make rem PNop)) acc_v
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
            lazy (V.map (fun x -> Segment x)
                    (make_straight V.empty X.num_ops V.empty stack)) in
          function
          | (IRE.L irm_l) ->
            let inner =  LinLoop (go (dep + 1) @@
                                  (IRE.P PPush) :: (IRE.P PPop) :: irm_l) in
            (dep, [], V.concat acc (Lazy.force prev_straight_v)
                      |> V.append inner)
          | (IRE.I IRE.EOP) ->
            (dep, [], V.concat acc (Lazy.force prev_straight_v)
                      |> V.append LinEOP)
          | x -> (dep, x :: stack, acc) in
        let (_, _, acc) = List.fold_left f (dep, [], V.empty) irml in
        acc
      in
      go 0

    type turn = CW | ACW
    let reverse = function
      | CW -> ACW
      | ACW -> CW

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

    let meta_chunk = function
      | Stick v -> (V.length v, 1, Stick v)
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
      cost  = max_float;
      inner = [];
    }

    let wh_cost phi w h =
      let r = float h /. float w in
      let a = float w *. float h in
      a *. (abs_float @@ (if r >= 1. then r else 1./.r) -. phi)**2.

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
            let inner = Row V.empty
                        :: Turn (List.hd info.row_h)
                        :: info.inner in
            let inner =
              let rec g acc = function
                | 0 -> acc
                | k -> g (Row V.empty :: Turn 1 :: acc) (k - 1) in
              g inner (n - 1)
            in
            let tot_h = info.tot_h + n in
            let info = {
              info with tot_h;
                        row_h = List.make n 1 @ info.row_h;
                        n_row = info.n_row + n;
                        blank = (tot_w - 1)
                                :: List.make (n-1) (tot_w - 2)
                                @ r :: info.blank;
                        cost = info.cost +. wh_cost phi tot_w tot_h;
                        inner;
            } in
            (false, info)

          | ((w, h, dc) :: t, n, r) when w > r ->
            ((w, h, dc) :: t, n - 1, init_rem (n - 1)) |> f {
              info with tot_h = info.tot_h + 1;
                        row_h = 1 :: info.row_h;
                        n_row = 1 + info.n_row;
                        blank = r :: info.blank;
                        inner = Turn (List.hd info.row_h) :: info.inner;
            }

          | ((w, h, dc) :: t, n, r) ->
            let (tot_h, row_h) = match info.row_h with
              | head :: tail->
                let cur_h = max h head in
                (info.tot_h + cur_h - head, (max h head) :: tail)
              | [] -> (info.tot_h + h, [h])
            in
            let cost = info.cost +. match dc with
              | Fence f_info -> f_info.cost
              | _ -> 0. in
            let inner = match info.inner with
              | Row dcv :: tl -> Row (V.append dc dcv) :: tl
              | tl -> Row (V.singleton dc) :: tl in
            f {info with tot_h; row_h; cost; inner;} (t, n, r - w)
        in
        f info (pmeta, n, init_rem n) in
      let (leftover, info) =
        fill n (tot_w, V.to_list prog_meta, {empty_info with width = tot_w;}) in
      if leftover then info_of_n prog_meta phi n (tot_w + 1) else info

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
end

(* let printcv = print_endline % V.foldi (fun i a b -> *)
(*     a ^ (Printf.sprintf "%d, %s \n" i @@ a ^ (show_colour b))) "" *)

(* let print_irm = print_endline % List.fold_lefti (fun a i b -> *)
(*     a ^ (Printf.sprintf "%d, %s \n" i @@ NaiveIRExpansion.show_ir_mix b)) "" *)

(* let paint_linear i = *)
(*   let x = NaiveIRExpansion.expand i in *)
(*   let _ = print_irm x in *)
(*   CanonicalDraw.draw_linear x *)

let paint_linear = CanonicalDraw.draw_linear % IRExpansion.Naive.expand
let paint_linear_fast ~stack_size = CanonicalDraw.draw_linear
                                    % IRExpansion.Fast.expand stack_size
