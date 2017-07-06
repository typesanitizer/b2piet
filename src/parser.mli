type error

val error_msg : error -> string

(**
   Takes in a string of symbols and a flag for indicating whether loops are
   present on missing and returns a list of Brainfuck instructions with
   positions (to be used by later passes for warnings, if any) and a list of
   errors.
*)
val parse : string -> bool ->
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list * error list
