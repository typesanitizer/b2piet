module FilePos : sig
  type pos
  val (<.) : pos -> pos -> bool
  val (>.) : pos -> pos -> bool
  val make_pos   : int -> int -> int -> pos
  val pos_to_str : pos -> string
end

module BFInstr : sig
  type bf_instr = Left | Right | Incr | Decr | Input | Output | Loop | Loopend
  val instr_to_char : bf_instr -> char
  val char_to_instr : char -> bf_instr option
end
