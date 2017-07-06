(**
   Operations and a type for positions inside source text.
*)
module FilePos : sig
  (**
     Type for describing file positions: useful for printing errors and
      warnings.
  *)
  type pos

  (** Check order for two positions.*)
  val (<.) : pos -> pos -> bool

  (** Check order for two positions.*)
  val (>.) : pos -> pos -> bool

  (**
     Takes the line number, position of the last newline from the file start,
     and the current position, in that order.
  *)
  val make_pos   : int -> int -> int -> pos
  val pos_to_str : pos -> string
end

(**
   Brainfuck instructions and associated functions.
*)
module BFInstr : sig
  type bf_instr = Left | Right | Incr | Decr | Input | Output | Loop | Loopend
  val instr_to_char : bf_instr -> char
  val char_to_instr : char -> bf_instr option
end

(**
   Piet IR type and a function for printing an IR tree.
*)
module PietIR : sig
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

  val print_ast : ir list -> unit
end

(**
   Description of and operations on Piet code.
*)
module Piet : sig

  (** Indices describing positions inside the program. *)
  type xy = int * int

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
  type codel = colour * xy

  (** Piet operations. *)
  type op =         PPush | PPop
          | PAdd  | PSub  | PMul
          | PDiv  | PMod  | PNot
          | PGrt  | PPtr  | PSwt
          | PDup  | PRoll | PInpN
          | POutN | PInpC | POutC

  (** Gives the next colour given the current colour and desired operation. *)
  val op_next_colour : op -> colour -> colour

  val show_colour : colour -> string
  val show_op : op -> string
end

type ord = EQ | GT | LT

module type ContainerOrd = sig
  type t
  type s
  val t_compare : t -> t -> ord
  val s_inside_t : s -> t -> ord
end

module SplayTree (M : ContainerOrd) : sig
  type t
  val snip_left  : t -> t * t
  val snip_right : t -> t * t
  val splay  : M.t -> t -> t
  val insert : M.t -> t -> t
  val find_s       : M.s -> t -> M.t option
  val find_s_splay : M.s -> t -> M.t option * t
end
