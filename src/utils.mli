val golden_ratio : float

type ord = EQ | GT | LT

(**
   A copy of BatVect with a pretty printing function.
*)
module Vect : sig
  type 'a t = 'a BatVect.t

  val pp :
    ?pp_sep : (Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> 'b) ->
    Format.formatter -> 'a t -> unit
end

(**
   A copy of BatArray with a pretty printing function.
*)
module Array : sig
  type 'a t = 'a BatArray.t

  val pp :
    ?pp_sep : (Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> 'b) ->
    Format.formatter -> 'a t -> unit
end

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
  val pp_op : Format.formatter -> op -> unit
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
          | Eop
          | Op of Piet.op

  val show_ir : ir -> string
  val print_ast : ir list -> unit
end

(**
   Describes containers with a total ordering. E.g. t = int range, s = int.
*)
module type U = sig
  type t
  type s
  val t_compare : t -> t -> ord
  val s_inside_t : s -> t -> ord
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module SplayTree (M : U) : sig
  type t
  val show : t -> string
  val empty : t
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

module type S = sig
  val num_ops : int
  val panel_to_rule_size_ratio : float
end

(**
   Two kinds of dimensions with utility functions.
   `codeldim` represents absolute dimensions in the image in terms of codels.
   `boxdim` represents relative dimensions in terms of number of rules (lines)
   used.
*)
module Dim : sig
  type codeldim = Codeldim of int
  val int_of_codeldim : codeldim -> int
  val map_codeldim : (int -> int) -> codeldim -> codeldim
  val add_codeldim : codeldim -> codeldim -> codeldim
  val sub_codeldim : codeldim -> codeldim -> codeldim

  type boxdim = Boxdim of int
  val int_of_boxdim : boxdim -> int
  val map_boxdim : (int -> int) -> boxdim -> boxdim
  val add_boxdim : boxdim -> boxdim -> boxdim
  val sub_boxdim : boxdim -> boxdim -> boxdim
  val pp_boxdim : Format.formatter -> boxdim -> unit
end

(**
   Deciding positions for placing vertical and horizontal rules.
*)
module RuleLoc(X: S) : sig
  type boxdim = Dim.boxdim
  type codeldim = Dim.codeldim

  val num_ops : codeldim

  val rule_loc_tbl : boxdim ->
    (boxdim, (codeldim * codeldim list) list) Hashtbl.t

  val dummy_rule_locs :
    (boxdim, (codeldim * codeldim list) list) Hashtbl.t ->
    ?random : bool ->
    nrule : boxdim ->
    len : codeldim ->
    codeldim list option

  (**
     Creates a basic grid of `nx` vertical rules and `ny` horizontal rules.
     `aspect` specifies the vertical / horizontal aspect ratio
     desired from the image (note: it is usually specified the other way
     around for televisions etc.).
     The output includes the total width, the total height, the x positions of
     the nx vertical rules and the y positions of the ny horizontal rules, in
     that order. Rule positions are specified for the left/top as relevant.
  *)
  val simple_grid :
    ?aspect : float ->
    nx : boxdim ->
    ny : boxdim ->
    (codeldim * codeldim) * (codeldim list * codeldim list)

  val blanked_grid :
    phi : float ->
    random : bool ->
    width : boxdim ->
    height : boxdim ->
    int list ->
    (codeldim * codeldim) * (codeldim list list * codeldim list)
end

module MetaJson : sig
  type t
  val empty : t
  val get : t
  val save : t -> unit
  val get_fast_push_table :
    ?use_json : bool ->
    ?num_ops : int ->
    stack_size : int -> t ->
    t * (int * int * FastPush.push_op list) list
  val get_ssize : string -> t -> string * int option
  val set_ssize : string -> int -> t -> t
end

