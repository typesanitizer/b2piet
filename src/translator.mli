type runtime_err

(**
   Takes a list of instructions and an argument specifying if condensation
   ought to be performed. If it is true, `++` would be translated to
   `add two` rather than `add 1, add 1`. Similarly, a sequence of 256
   decrements would be translated to a no-op.
*)
val translate :
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list ->
  bool -> bool -> int -> bool ->
  (Utils.PietIR.ir list * runtime_err option)
