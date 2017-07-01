type error

val error_msg : error -> string
val parse : string -> bool ->
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list * error list
