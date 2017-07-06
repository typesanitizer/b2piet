open Batteries

open Utils.Piet

module V = BatVect
type 'a vec = 'a V.t
type picture = colour vec vec * int * int

type ir = Utils.PietIR.ir

let pprint = print_endline % Printf.sprintf

module NaiveIRExpansion = struct

  module PI = Utils.PietIR

  type ir_do = White | Random | Cp of int | Grow of int | Eop
  [@@deriving show]

  type ir_mix = P of op | I of ir_do | L of ir_mix list

  let rec show_ir_mix = function
    | L irml -> (Printf.sprintf "L: %s" @@ List.fold_lefti
                   (fun acc i b -> acc ^ (Printf.sprintf
                                            "    %s, %s\n"
                                            (string_of_int i)
                                            (show_ir_mix b))) "" irml)
    | I ird -> show_ir_do ird
    | P o -> show_op o

  let rec expand_ir acc = function
    | PI.Input  -> P PInpC :: acc
    | PI.Output -> P POutC :: P PDup :: acc
    | PI.Not    -> P PNot  :: acc
    | PI.White  -> I White :: acc
    | PI.Random -> I Random :: acc
    | PI.Cp a   -> I (Cp a) :: acc
    | PI.Grow a -> I (Grow a) :: acc
    | PI.Push a ->
      (* Unnecessary compiler warnings D:. *)
      (match compare a 0 with
       |  1 -> (* I White :: *) P PPush :: expand_ir acc (PI.Grow a)
       |  0 -> (* I White :: *) P PNot :: expand_ir acc (PI.Push 1)
       | -1 -> (* I White :: *) P PSub :: expand_ir (expand_ir acc (PI.Push 1)) (PI.Push (-a+1)))
    | PI.Add a  -> P PAdd :: expand_ir acc (PI.Push a)
    | PI.Subtract a -> P PSub :: expand_ir acc (PI.Push a)
    | PI.Multiply a -> P PMul :: expand_ir acc (PI.Push a)
    | PI.Mod a      -> acc (*P PMod :: expand_ir acc (PI.Push a)*)
    | PI.Roll (a,b) -> P PRoll ::
                       expand_ir (expand_ir acc (PI.Push b)) (PI.Push a)
    | PI.Loop ir_l  -> L (List.rev @@ List.fold_left expand_ir [] ir_l) :: acc
    | PI.Eop        -> I Eop :: acc

  let expand = List.rev % List.fold_left expand_ir []
end

module CanonicalDraw = struct

  module IR = Utils.PietIR
  module IRE = NaiveIRExpansion

  type ir_block = xy * bool * picture

  let rec fill_rows : int -> picture -> picture = fun cur_y (clr_a, w, h) ->
    if cur_y = h then (clr_a, w, h)
    else
      let cur_row = V.get clr_a cur_y in
      let black_row = V.make (w - V.length cur_row) Black in
      fill_rows (cur_y+1)
        (V.modify clr_a cur_y (fun r -> V.concat r black_row), w, h)

  let fill_below = fill_rows 1

  let rec draw_linear_right :
    int * colour * int * picture -> IRE.ir_mix -> int * colour * int * picture =

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
      | IRE.P op ->
        if cur_clr = White then
          let rand_clr = Green (* 18 |> Random.int |> num_to_colour *) in
          let next_clr = op_next_colour op rand_clr in
          let tl = V.singleton rand_clr |> V.append next_clr in
          (depth, next_clr, 1, concat_1d 2 tl)
        else
          let next_clr = op_next_colour op cur_clr in
          (depth, next_clr, 1, append_1 next_clr)

      | IRE.I IRE.White  ->
        if cur_clr = White then (depth, Blue, 1, append_1 Blue)
        else (depth, White, 1, append_1 White)
      | IRE.I IRE.Random ->
        let rand_clr = Red (*18 |> Random.int |> num_to_colour*) in
        (depth, rand_clr, 1, append_1 rand_clr)

      (* assert a > 0 *)
      | IRE.I IRE.Cp a ->
        (depth, cur_clr, a+1, cur_clr |> V.make a |> concat_1d a)

      (* assert a > 0 *)
      | IRE.I IRE.Grow a  ->
        (if cur_clr = White then
           let rand_clr = Blue in
           draw_linear_right
             (depth, rand_clr, 1, append_1 rand_clr) (IRE.I (IRE.Grow a))
         else
           let acc = (depth, cur_clr, sz, (p_clr_a2d, w, h)) in
           match compare sz a with
           | 1  -> List.fold_left
                     draw_linear_right
                     acc [IRE.I IRE.White; IRE.I IRE.Random; IRE.I (IRE.Cp (a-1))]
           | -1 -> draw_linear_right acc (IRE.I (IRE.Cp (a-sz)))
           | 0  -> acc)

      | IRE.I IRE.Eop ->
        let temp =
          V.of_list [
            V.of_list [White; White; Green; Black;  Cyan;];
            V.of_list [Black; Black; White; Black;  Cyan;];
            V.of_list [Black; Green; Green; Green; Black;];
            V.of_list [Black; Black; Black; Black;  Cyan;];
          ]
        in
        (depth, White, 1, concat_2d (p_clr_a2d, w, h) (temp, 5, 4))

      | IRE.L (irm_l) ->
        let (_,_, _, (c_clr_a, c_w, c_h)) =
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
              make_vec @@ (IRE.expand [IR.Push 3]) @ [IRE.P PPtr] in
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
            make_vec [IRE.P PDup; IRE.P PNot; IRE.P PNot; IRE.P PPtr]
          else
            make_vec @@ [IRE.P PDup; IRE.P PNot; IRE.P PNot;]
                        @ (IRE.expand [IR.Multiply 3])
                        @ [IRE.P PPtr] in
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
        (draw_linear_right)
        (0, White, 0, (V.singleton V.empty, 0, 1))
        irm_l in
    full_pic
end

(* let printcv = print_endline % V.foldi (fun i a b -> *)
(*     a ^ (Printf.sprintf "%d, %s \n" i @@ a ^ (show_colour b))) "" *)

(* let print_irm = print_endline % List.fold_lefti (fun a i b -> *)
(*     a ^ (Printf.sprintf "%d, %s \n" i @@ NaiveIRExpansion.show_ir_mix b)) "" *)

(* let paint_linear i = *)
(*   let x = NaiveIRExpansion.expand i in *)
(*   let _ = print_irm x in *)
(*   CanonicalDraw.draw_linear x *)

let paint_linear = CanonicalDraw.draw_linear % NaiveIRExpansion.expand
