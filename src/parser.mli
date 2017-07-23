type error

val error_msg : error -> string

(**
   Takes in a string of symbols and a flag for indicating whether loops are
   present or missing and returns a list of Brainfuck instructions with
   positions (to be used by later passes for warnings, if any) and a list of
   errors. (Yes, I know this is obvious from the type signature but I just put
   this in because I wasn't sure what to write...)
*)
val parse : ?loops_off:bool -> string ->
  (Utils.BFInstr.t * Utils.FilePos.t) list * error list
