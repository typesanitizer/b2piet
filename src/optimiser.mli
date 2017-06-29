type warning

val warn_msg : warning -> string
val optimise :
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list ->
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list * warning list
