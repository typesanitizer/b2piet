type warning

val warn_msg : warning -> string

(**
   Takes in parsed input and returns optimised output with possible warnings.
*)
val optimise :
  (Utils.BFInstr.t * Utils.FilePos.t) list ->
  (Utils.BFInstr.t * Utils.FilePos.t) list * warning list
