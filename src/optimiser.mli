type warning

val warn_msg : warning -> string

(**
   Takes in parsed input and returns optimised output with possible warnings.
*)
val optimise :
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list ->
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list * warning list
