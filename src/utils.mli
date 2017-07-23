val golden_ratio : float

type ord = EQ | GT | LT

(**
   Operations and a type for positions inside source text.
*)
module FilePos : sig
  (**
     Type for describing file positions: useful for printing errors and
      warnings.
  *)
  type t

  (** Check order for two positions.*)
  val (<.) : t -> t -> bool

  (** Check order for two positions.*)
  val (>.) : t -> t -> bool

  (**
     Takes the line number, position of the last newline from the file start,
     and the current position, in that order.
  *)
  val make_pos   : int -> int -> int -> t
  val pos_to_str : t -> string
end

(**
   Brainfuck instructions and associated functions.
*)
module BFInstr : sig
  type t = Left | Right | Incr | Decr | Input | Output | Loop | Loopend
  val instr_to_char : t -> char
  val char_to_instr : char -> t option
end

(**
   Piet IR type and a function for printing an IR tree.
*)
module PietIR : sig
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

  val print_ast : ir list -> unit
end

(**
   Description of and operations on Piet code.
*)
module Piet : sig

  (** Indices describing positions inside the program. *)
  type xy = int * int

  val (<@) : xy -> xy -> bool
  val (>@) : xy -> xy -> bool
  val compare_xy : xy -> xy -> ord
  (** Allowed colours in Piet programs. *)
  type colour =
      White | Black
    | LightRed     | Red     | DarkRed
    | LightYellow  | Yellow  | DarkYellow
    | LightGreen   | Green   | DarkGreen
    | LightCyan    | Cyan    | DarkCyan
    | LightBlue    | Blue    | DarkBlue
    | LightMagenta | Magenta | DarkMagenta

  (** Colour definitions in standard RGB format (e.g. 0x09FA31). *)
  val colour_to_hex : colour -> int
  (** Inverse transformation of `colour_to_hex`. *)
  val hex_to_colour : int -> colour

  (** A Piet codel. *)
  type codel = colour * int * int

  (** Piet operations. *)
  type op = PNop  | PPush | PPop
          | PAdd  | PSub  | PMul
          | PDiv  | PMod  | PNot
          | PGrt  | PPtr  | PSwt
          | PDup  | PRoll | PInpN
          | POutN | PInpC | POutC

  (** Gives the next colour given the current colour and desired operation. *)
  val op_next_colour : op -> colour -> colour
  (** Gives the previous colour given the current colour and desired operation. *)
  val op_prev_colour : op -> colour -> colour

  val show_colour : colour -> string
  val show_op : op -> string
end

module type OrdEqClass = sig
  type t
  type s
  val t_compare : t -> t -> ord
  val s_inside_t : s -> t -> ord
end

module SplayTree (M : OrdEqClass) : sig
  type t
  val snip_left  : t -> t * t
  val snip_right : t -> t * t
  val splay  : M.t -> t -> t
  val insert : M.t -> t -> t
  val to_vec : t -> M.t BatVect.t
  val find_s_nosplay : M.s -> t -> M.t option
  val find_s: M.s -> t -> M.t option * t
end

module FastPush : sig
  type binary_op = PAdd | PSub | PMul | PDiv | PMod
  type push_op = Number of int | PDup | Binary of binary_op
  val fast_push : int -> int -> (int * int * push_op list) list
  val fast_push_rev : int -> int -> (int * int * push_op list) list
  val fast_push_str : int -> int -> (int * int * string) list
end
