open Batteries
open Images

module FilePos = struct
  type pos = int * int * int

  let (<.) = fun (_,_,c1) (_,_,c2) -> c1 < c2
  let (>.) = fun (_,_,c1) (_,_,c2) -> c1 > c2
  let make_pos x y z = (x,y,z)
  let pos_to_str (l,lsn,cn) = let to_str = BatInt.to_string in
    "(line:" ^ (to_str l) ^ ",char:" ^ (to_str (cn-lsn)) ^ ")"
end

module BFInstr = struct
  type bf_instr = Left | Right | Incr | Decr | Input | Output | Loop | Loopend

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
          | White
          | Random
          | Cp of int
          | Grow of int
          | Push of int
          | Add of int
          | Subtract of int
          | Multiply of int
          | Mod of int
          | Roll of int * int
          | Loop of ir list
          | Eop
  [@@deriving show] (* using ppx_deriving *)
  let print_ast ir_l = List.iter (print_endline % show_ir) ir_l
end

module Piet = struct
  type xy = int * int

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

  let (+?) (h1, d1) (h2, d2) =
    if h1 < 0 || h2 < 0 then raise Monochrome_Addition
    else ((h1 + h2) mod 6, (d1 + d2) mod 3)

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

  type codel = colour * xy

  type op =         PPush | PPop
          | PAdd  | PSub  | PMul
          | PDiv  | PMod  | PNot
          | PGrt  | PPtr  | PSwt
          | PDup  | PRoll | PInpN
          | POutN | PInpC | POutC
          [@@deriving show]

  let op_to_delta = function
                      | PPush -> (0, 1) | PPop  -> (0, 2)
    | PAdd  -> (1, 0) | PSub  -> (1, 1) | PMul  -> (1, 2)
    | PDiv  -> (2, 0) | PMod  -> (2, 1) | PNot  -> (2, 2)
    | PGrt  -> (3, 0) | PPtr  -> (3, 1) | PSwt  -> (3, 2)
    | PDup  -> (4, 0) | PRoll -> (4, 1) | PInpN -> (4, 2)
    | POutN -> (5, 0) | PInpC -> (5, 1) | POutC -> (5, 2)

  let op_next_colour op c = hd_to_colour @@ op_to_delta op +? colour_to_hd c

end

type ord = EQ | GT | LT

module type ContainerOrd = sig
  type t
  type s
  val t_compare : t -> t -> ord
  val s_inside_t : s -> t -> ord
end

module SplayTree (M : ContainerOrd) = struct
  type t = Empty | Node of t * M.t * t

  exception Unreachable

  let snip_left : t -> t * t = function
    | Empty -> (Empty, Empty)
    | Node (l, n, r) -> (l, Node (Empty, n, r))
  let snip_right = function
    | Empty -> (Empty, Empty)
    | Node (l, n, r) -> (Node (l, n, Empty), r)

  (* NULL instead of option type so that pattern matching looks better. *)
  type direction = L | R | NULL

  (* helper functions for splaying and find+splay operation*)
  let rec path = function
    | (x, Empty, ps) -> ps
    | (x, Node (l, n, r), ps) ->
      match M.t_compare x n with
      | EQ -> ps
      | LT -> path (x,l,(L,l)::ps)
      | GT -> path (x,r,(R,r)::ps)

  (* Wikipedia has a nicer explanation than I can write here:
     https://en.wikipedia.org/wiki/Splay_tree . *)
  let rec rebuild : (direction * t) list -> t =

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
    | _ -> raise Unreachable

  let splay x st = rebuild @@ path (x, st, [(NULL, st)])

  let insert x st =
    match (splay x st) with
    | Empty -> Node (Empty, x, Empty)
    | Node (l, v, r) ->
      match M.t_compare x v with
      | EQ -> st
      | LT -> let (l, r) = snip_left st in Node (l, x, r)
      | GT -> let (l, r) = snip_right st in Node (l, x, r)

  (* NOTE: doesn't rearrange the tree. *)
  let rec find_s : M.s -> t -> M.t option = fun s -> function
    | Empty -> None
    | Node (l, v, r) ->
      match M.s_inside_t s v with
      | EQ -> Some v
      | LT -> find_s s l
      | GT -> find_s s r

  let rec find_s_splay : M.s -> t -> M.t option * t = fun s ->
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
      | Empty -> raise Unreachable
      | Node (l, v2, r) ->
        if M.s_inside_t s v2 = EQ then (Some v2, Node (l,v2,r))
        else (None, Node (l,v2,r))
end
