open Batteries
open Images

let golden_ratio = 1.618

type ord = EQ | GT | LT

module FilePos = struct
  type t = int * int * int

  let (<.) = fun (_, _, c1) (_, _, c2) -> c1 < c2
  let (>.) = fun (_, _, c1) (_, _, c2) -> c1 > c2
  let make_pos x y z = (x,y,z)
  let pos_to_str (l,lsn,cn) = let to_str = BatInt.to_string in
    "(line:" ^ (to_str l) ^ ",char:" ^ (to_str (cn-lsn)) ^ ")"
end

module BFInstr = struct
  type t = Left | Right | Incr | Decr | Input | Output | Loop | Loopend

  let instr_to_char = function
    | Left    -> '<'
    | Right   -> '>'
    | Incr    -> '+'
    | Decr    -> '-'
    | Input   -> ','
    | Output  -> '.'
    | Loop    -> '['
    | Loopend -> ']'
  let char_to_instr = function
    | '<' -> Some Left
    | '>' -> Some Right
    | '+' -> Some Incr
    | '-' -> Some Decr
    | ',' -> Some Input
    | '.' -> Some Output
    | '[' -> Some Loop
    | ']' -> Some Loopend
    | _   -> None
end

module PietIR = struct
  type ir = Input
          | Output
          | Not
          | Push of int
          | Add of int
          | Subtract of int
          | Multiply of int
          | Mod of int
          | Roll of int * int
          | Loop of ir list
          | Dup
          | Eop
  [@@deriving show] (* using ppx_deriving *)
  let print_ast ir_l = List.iter (print_endline % show_ir) ir_l
end

(* module BFInterpreter = struct *)
(* end *)

module Piet = struct
  type xy = int * int

  let (<@) (x1, y1) (x2, y2) =
    if x1 = x2 then y1 < y2
    else x1 < x2
  let (>@) (x1, y1) (x2, y2) =
    if x1 = x2 then y1 > y2
    else x1 > x2
  let compare_xy (x1, y1) (x2, y2) =
    if x1 = x2 then
      match compare y1 y2 with
      | x when x > 0 -> GT
      | 0 -> EQ
      | _ -> LT
    else if x1 > x2 then GT
    else LT

  exception Monochrome_Addition
  exception HD_Out_of_Bounds

  type colour =
      White | Black
    | LightRed     | Red     | DarkRed
    | LightYellow  | Yellow  | DarkYellow
    | LightGreen   | Green   | DarkGreen
    | LightCyan    | Cyan    | DarkCyan
    | LightBlue    | Blue    | DarkBlue
    | LightMagenta | Magenta | DarkMagenta
  [@@deriving show]

  let colour_to_hex = function
    | White        -> 0xFFFFFF | Black   -> 0x000000 (* Grey 0x6C7B8B*)
    | LightRed     -> 0xFFC0C0 | Red     -> 0xFF0000 | DarkRed     -> 0xC00000
    | LightYellow  -> 0xFFFFC0 | Yellow  -> 0xFFFF00 | DarkYellow  -> 0xC0C000
    | LightGreen   -> 0xC0FFC0 | Green   -> 0x00FF00 | DarkGreen   -> 0x00C000
    | LightCyan    -> 0xC0FFFF | Cyan    -> 0x00FFFF | DarkCyan    -> 0x00C0C0
    | LightBlue    -> 0xC0C0FF | Blue    -> 0x0000FF | DarkBlue    -> 0x0000C0
    | LightMagenta -> 0xFFC0FF | Magenta -> 0xFF00FF | DarkMagenta -> 0xC000C0

  type hd = int * int

  let num_to_hd n = (n mod 6, n mod 3)

  let (+?) : hd -> hd -> hd = fun (h1, d1) (h2, d2) ->
    if h1 < 0 || h2 < 0 then raise Monochrome_Addition
    else ((h1 + h2) mod 6, (d1 + d2) mod 3)
  let (-?) (h1, d1) (h2, d2) =
    if h1 < 0 || h2 < 0 then raise Monochrome_Addition
    else ((6 + h1 - h2) mod 6, (3 + d1 - d2) mod 3)

  let colour_to_hd = function
    | LightRed     -> (0, 0) | Red     -> (0, 1) | DarkRed     -> (0, 2)
    | LightYellow  -> (1, 0) | Yellow  -> (1, 1) | DarkYellow  -> (1, 2)
    | LightGreen   -> (2, 0) | Green   -> (2, 1) | DarkGreen   -> (2, 2)
    | LightCyan    -> (3, 0) | Cyan    -> (3, 1) | DarkCyan    -> (3, 2)
    | LightBlue    -> (4, 0) | Blue    -> (4, 1) | DarkBlue    -> (4, 2)
    | LightMagenta -> (5, 0) | Magenta -> (5, 1) | DarkMagenta -> (5, 2)
    | White        -> (-1,0) | Black   -> (-1,1)

  let hd_to_colour = function
    | (0, 0) -> LightRed     | (0, 1) -> Red     | (0, 2) -> DarkRed
    | (1, 0) -> LightYellow  | (1, 1) -> Yellow  | (1, 2) -> DarkYellow
    | (2, 0) -> LightGreen   | (2, 1) -> Green   | (2, 2) -> DarkGreen
    | (3, 0) -> LightCyan    | (3, 1) -> Cyan    | (3, 2) -> DarkCyan
    | (4, 0) -> LightBlue    | (4, 1) -> Blue    | (4, 2) -> DarkBlue
    | (5, 0) -> LightMagenta | (5, 1) -> Magenta | (5, 2) -> DarkMagenta
    | (-1, 0) -> White       | (-1, 1) -> Black  | _ -> raise HD_Out_of_Bounds

  let hex_to_colour n = n |> num_to_hd |> hd_to_colour

  type codel = colour * int * int

  type op = PNop  | PPush | PPop
          | PAdd  | PSub  | PMul
          | PDiv  | PMod  | PNot
          | PGrt  | PPtr  | PSwt
          | PDup  | PRoll | PInpN
          | POutN | PInpC | POutC
  [@@deriving show]

  let op_to_delta = function
    | PNop  -> (0, 0) | PPush -> (0, 1) | PPop  -> (0, 2)
    | PAdd  -> (1, 0) | PSub  -> (1, 1) | PMul  -> (1, 2)
    | PDiv  -> (2, 0) | PMod  -> (2, 1) | PNot  -> (2, 2)
    | PGrt  -> (3, 0) | PPtr  -> (3, 1) | PSwt  -> (3, 2)
    | PDup  -> (4, 0) | PRoll -> (4, 1) | PInpN -> (4, 2)
    | POutN -> (5, 0) | PInpC -> (5, 1) | POutC -> (5, 2)

  let op_next_colour op c = hd_to_colour @@ op_to_delta op +? colour_to_hd c
  let op_prev_colour op c = hd_to_colour @@ op_to_delta op -? colour_to_hd c

end

module type OrdEqClass = sig
  type t
  type s
  val t_compare : t -> t -> ord
  val s_inside_t : s -> t -> ord
end

module SplayTree (M : OrdEqClass) = struct
  type t = Empty | Node of t * M.t * t

  let snip_left : t -> t * t = function
    | Empty -> (Empty, Empty)
    | Node (l, n, r) -> (l, Node (Empty, n, r))
  let snip_right = function
    | Empty -> (Empty, Empty)
    | Node (l, n, r) -> (Node (l, n, Empty), r)

  (* NULL instead of option type so that pattern matching looks better. *)
  type direction = L | R | NULL

  (* Helper function(s) for splaying and find + splay operation. *)
  let rec path = function
    | (x, Empty, ps) -> ps
    | (x, Node (l, n, r), ps) ->
      match M.t_compare x n with
      | EQ -> ps
      | LT -> path (x,l,(L,l)::ps)
      | GT -> path (x,r,(R,r)::ps)

  (* Wikipedia has a nicer explanation than I can write here:
     https://en.wikipedia.org/wiki/Splay_tree . *)
  let [@warning "-8"] rec rebuild : (direction * t) list -> t =

    let zig_l (Node (lc, c, rc)) (Node (_, p, rp)) =
      Node (lc, c, Node (rc, p, rp)) in

    let zig_r (Node (lc, c, rc)) (Node (lp, p, _)) =
      Node (Node (lp, p, lc), c, rc) in

    let zigzig_l (Node (lc, c, rc)) (Node (_, p, rp)) (Node (_, g, rg)) =
      Node (lc, c, Node (rc, p, Node(rp, g, rg))) in

    let zigzig_r (Node (lc, c, rc)) (Node (lp, p, _)) (Node (lg, g, _)) =
      Node (Node (Node (lg, g, lp), p, lc), c, rc) in

    let zigzag_l (Node (lc, c, rc)) (Node (lp, p, _)) (Node (_, g, rg)) =
      Node (Node (lp, p, lc), c, Node (rc, g, rg)) in

    let zigzag_r (Node (lc, c, rc)) (Node (_, p, rp)) (Node (lg, g, _)) =
      Node (Node (lg, g, lc), c, Node (rc, p, rp)) in

    function
    | [(NULL, tr)] -> tr
    | [(L, c); (_, p)] -> zig_l c p
    | [(R, c); (_, p)] -> zig_r c p
    | (L, c)::(L, p)::(d, g)::ps -> (d, zigzig_l c p g)::ps |> rebuild
    | (R, c)::(R, p)::(d, g)::ps -> (d, zigzig_r c p g)::ps |> rebuild
    | (R, c)::(L, p)::(d, g)::ps -> (d, zigzag_l c p g)::ps |> rebuild
    | (L, c)::(R, p)::(d, g)::ps -> (d, zigzag_r c p g)::ps |> rebuild
    | _ -> raise (Failure "Weird condition triggered in rebuild.")

  let splay x st = rebuild @@ path (x, st, [(NULL, st)])

  let insert x st =
    match (splay x st) with
    | Empty -> Node (Empty, x, Empty)
    | Node (l, v, r) ->
      match M.t_compare x v with
      | EQ -> st
      | LT -> let (l, r) = snip_left st in Node (l, x, r)
      | GT -> let (l, r) = snip_right st in Node (l, x, r)

  let rec to_vec = function
    | Empty -> BatVect.empty
    | Node (l, n, r) -> to_vec r
                        |> BatVect.concat (to_vec l |> BatVect.append n)

  (* NOTE: doesn't rearrange the tree. *)
  let rec find_s_nosplay : M.s -> t -> M.t option = fun s -> function
    | Empty -> None
    | Node (l, v, r) ->
      match M.s_inside_t s v with
      | EQ -> Some v
      | LT -> find_s_nosplay s l
      | GT -> find_s_nosplay s r

  let rec find_s : M.s -> t -> M.t option * t = fun s ->
    function
    | Empty -> (None, Empty)
    | stree ->
      let rec find_path = function
        | (s, Empty, ps) -> ps
        | (s, Node (l, v, r), ps) ->
          match M.s_inside_t s v with
          | EQ -> ps
          | LT -> find_path (s, l, (L,l)::ps)
          | GT -> find_path (s, r, (R,r)::ps) in
      match rebuild @@ find_path (s, stree, [(NULL, stree)]) with
      | Empty -> raise (Failure "Unexpected match in find_s.")
      | Node (l, v2, r) ->
        if M.s_inside_t s v2 = EQ then (Some v2, Node (l,v2,r))
        else (None, Node (l,v2,r))
end

module FastPush = struct
  module H = BatHashtbl
  module V = BatVect

  type seq = int list
  let int_max = BatInt.max_num
  type binary_op = PAdd | PSub | PMul | PDiv | PMod
  type push_op = Number of int | PDup | Binary of binary_op

  let string_of_push_op = function
    | Number x -> string_of_int x
    | PDup -> "@"
    | Binary PAdd -> "+"
    | Binary PSub -> "-"
    | Binary PMul -> "*"
    | Binary PDiv -> "/"
    | Binary PMod -> "%"

  type full_history = {
    goal : int;          (* we are interested in numbers from 1 to goal *)
    def  : push_op list; (* default list of operations for child nodes  *)
    maxc : int ref;      (* max cost amongst numbers between 1 and goal *)
    best : (int, int * push_op list) H.t; (* number -> (cost, best path) *)
    hist : (seq, int) H.t;                (* sequence -> length of best path *)
  }

  type path_history = {
    cost : int;
    path : push_op list;
    st_l : seq list;     (* list of sequences for backtracking *)
  }

  let rec branch path_h full_h =
    (* If current sequence cannot be reduced to a singleton even after reaching
       the deepest level of the true, or if the current cost matches or exceeds
       the highest cost we have, there is no point in going further. *)
    if (not (List.is_empty path_h.st_l) &&
        (BatInt.max 0 (List.length (List.hd path_h.st_l) - 1)) + path_h.cost
        >= !(full_h.maxc)) then full_h
    else
      let make_kids acc = function
        | PDup -> (match path_h.st_l with
            | (h :: t) :: _ -> (PDup, path_h.cost + 1, h :: h :: t) :: acc
            | _ -> acc)
        | Binary x
          -> (match path_h.st_l with
              | (a :: b :: t) :: _ ->
                if ((b < 0 && x = PMod)
                    || (a = 0 && (x = PDiv || x = PMod))) then acc
                else
                  let ab_op = (match x with
                      | PAdd -> b + a
                      | PSub -> b - a
                      | PMul -> b * a
                      | PDiv -> b / a
                      | PMod -> b mod a) in
                  (Binary x, path_h.cost + 1, ab_op :: t) :: acc
              | _ -> acc)
        | Number x -> (Number x, path_h.cost + x,
                       (match path_h.st_l with
                        | [] -> [x]
                        | h :: _ -> x :: h)) :: acc
      in
      let traverse path_h fh (el, cost, seq) =
        if cost > !(fh.maxc) then fh
        else
          let old_c = match H.find_option fh.hist seq with
            | Some x -> x
            | None -> int_max in
          if cost >= old_c then fh
          else
            (H.replace fh.hist seq cost;
             if List.length seq = 1 then (
               let x = List.hd seq in
               if x >= 0 then
                 let old_c = match H.find_option fh.best x with
                   | Some (y, _) -> y
                   | None -> int_max in
                 if cost < old_c then (
                   H.replace fh.best x (cost, el :: path_h.path);
                   if old_c = !(fh.maxc) then
                     fh.maxc :=
                       List.range 1 `To fh.goal
                       |> List.map (fst % H.find fh.best)
                       |> List.reduce BatInt.max
                 ));
             branch {cost = cost; path = el :: path_h.path;
                     st_l = seq :: path_h.st_l} fh)
      in
      full_h.def
      |> List.fold_left make_kids []
      |> List.fold_left (traverse path_h) full_h

  let fast_push_rev max_num goal =
    let max_num = BatInt.min 5 max_num in
    let start_path_h = {cost = 0; path = []; st_l = [];} in
    let start_full_h guess = {
      best = (List.range 1 `To goal)
             |> List.map (fun x -> (x, (guess, [])))
             |> H.of_list;
      maxc = ref guess;
      hist = H.create 1024;
      def = [PDup;]
            |> List.append
               % List.map (fun x -> Binary x) @@ [PAdd; PSub; PMul; PDiv; PMod;]
            |> List.append
               % List.map (fun x -> Number x) @@ (List.range 1 `To max_num);
      goal = goal;
    } in

    let rec run_branch guess full_h =
      let fh = branch start_path_h full_h in
      if List.range 1 `To goal
         |> List.map ((=) guess % fst % H.find fh.best)
         |> List.reduce (||) then
        run_branch (guess + 2) (start_full_h (guess + 2))
      else fh
    in
    let full_h = run_branch 16 (start_full_h 16) in

    List.range 1 `To goal
    |> List.map (fun a -> let (c, p) = H.find full_h.best a in (a, c, p))
    |> List.sort (fun (a1, _, _) (a2, _, _) -> compare a1 a2)

  let fast_push m g = fast_push_rev m g
                      |> List.map (fun (a,c,l) -> (a, c, List.rev l))

  let fast_push_str max_num goal =
    fast_push max_num goal
    |> List.map (fun (a, c, p) ->
        (a, c, List.fold_left (^) "" @@ List.map (string_of_push_op) p))

end
